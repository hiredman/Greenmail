(ns greenmail.store
  (:require [clojure.set :as set]
            [clojure.stacktrace :as s])
  (:import (javax.mail Flags$Flag
                       Flags)
           (javax.mail.internet MimeMessage)
           (com.icegreen.greenmail.store MailFolder
                                         SimpleStoredMessage
                                         MessageFlags
                                         FolderListener
                                         InMemoryStore
                                         FolderException)
           (java.util UUID
                      StringTokenizer)))

(def mail (ref {}))

(defn clear-mail []
  (dosync
   (ref-set mail {})))

(defn agent-print-trace [_ e]
  (s/print-stack-trace e))

(defprotocol HasChildren
  (get-children [_])
  (get-child [_ child-id])
  (add-child [_ child])
  (remove-child [_ child]))

(defprotocol HasParent
  (get-parent [_]))

(defprotocol Nameable
  (set-name [_ new-name]))

(defprotocol SelectEnabling
  (set-selectable [_ v]))

(def hierarchy-delimiter-char ".")
(def user-namespace "#mail")

(def permament-flags
  (doto (Flags.)
    (.add Flags$Flag/ANSWERED)
    (.add Flags$Flag/DELETED)
    (.add Flags$Flag/DRAFT)
    (.add Flags$Flag/FLAGGED)
    (.add Flags$Flag/SEEN)))

(declare ->HiMF)

(defn get-full-name [id]
  (if (:root? (get @mail id))
    (:name (get @mail id))
    (str (get-full-name (:parent (get @mail id)))
         hierarchy-delimiter-char
         (:name (get @mail id)))))

(defn get-message-count [id]
  (count (:all (:messages (get @mail id)))))

(defn recent-count [id reset?]
  (apply + (for [{:keys [^SimpleStoredMessage message]} (:all (:messages (get @mail id)))
                 :when (.contains (.getFlags message) Flags$Flag/RECENT)]
             (do
               (when reset?
                 (.remove (.getFlags message) Flags$Flag/RECENT))
               1))))

(defn get-first-unseen [id]
  (or (first (for [{:keys [^SimpleStoredMessage message msn]} (:all (:messages (get @mail id)))
                   :when (not (.contains (.getFlags message) Flags$Flag/SEEN))]
               msn))
      -1))

(defn get-unseen-count [id]
  (apply + (for [{:keys [^SimpleStoredMessage message msn]} (:all (:messages (get @mail id)))
                 :when (not (.contains (.getFlags message) Flags$Flag/SEEN))]
             1)))

(defn messages-get [id k]
  (get (:messages (get @mail id)) k))

(defn get-msn [id uid]
  (:msn (first (messages-get id {:uid uid}))))

(defn signal-deletion [id]
  (let [a (agent nil)]
    (set-error-handler! a agent-print-trace)
    (dosync
     (ensure mail)
     (doseq [^FolderListener listener (:listeners (get @mail id))]
       (send-off a (fn [_] (.mailboxDeleted listener)))))
    (await a)))

(defn add-message [m ^SimpleStoredMessage msg]
  (let [uid (.getUid msg)
        msn (inc (count (:all m)))
        nm {:uid uid
            :msn msn
            :message msg}]
    (merge-with into
                m
                (set/index #{nm} [:uid])
                (set/index #{nm} [:msn])
                {:all #{nm}})))

(defn append-message [id message flags internal-date]
  (let [a (agent nil)
        _ (set-error-handler! a agent-print-trace)
        uid (dosync
             (let [uid (:next-uid (get @mail id))
                   smsg (SimpleStoredMessage. message flags internal-date uid)]
               (alter mail update-in [id :next-uid] inc)
               (.add (.getFlags smsg) Flags$Flag/RECENT)
               (alter mail update-in [id :messages] add-message smsg)
               (let [i (count (:all (:messages (get @mail id))))]
                 (doseq [^FolderListener listener (:listeners (get @mail id))]
                   (send-off a (fn [_] (.added listener i)))))
               uid))]
    (await a)
    uid))

(defn set-flags [id ^Flags flags value? uid silent-listener add-uid?]
  (let [{:keys [msn ^SimpleStoredMessage message]}
        (first (get (:messages (get @mail id)) {:uid uid}))]
    (if value?
      (.add (.getFlags message) flags)
      (.remove (.getFlags message) flags))
    (let [flags (.getFlags message)]
      (doseq [^FolderListener listener (:listeners (get @mail id))
              :when (not= listener silent-listener)]
        (.flagsUpdated listener msn flags (when add-uid? uid))))))

(defn replace-flags [id ^Flags flags uid silent-listener add-uid?]
  (let [{:keys [msn ^SimpleStoredMessage message]}
        (get (:messages (get @mail id)) {:uid uid})]
    (.remove (.getFlags message) MessageFlags/ALL_FLAGS)
    (.add (.getFlags message) flags)
    (let [flags (.getFlags message)]
      (doseq [^FolderListener listener (:listeners (get @mail id))
              :when (not= listener silent-listener)]
        (.flagsUpdated listener msn flags (when add-uid? uid))))))

(defn expunge [id]
  (let [a (agent nil)]
    (set-error-handler! a agent-print-trace)
    (dosync
     (let [{:keys [messages]} (get @mail id)
           {deleted true
            kept false} (group-by
                         (fn [{:keys [^SimpleStoredMessage message]}]
                           (.contains (.getFlags message) Flags$Flag/DELETED))
                         (:all messages))]
       (alter mail update-in [id] assoc :messages
              (reduce add-message {} (map :message kept)))
       (doseq [^FolderListener listener (:listeners (get @mail id))
               {:keys [msn]} deleted]
         (send-off a (fn [_] (.expunged listener msn))))))
    (await a)))

(defn ^SimpleStoredMessage get-message [id uid]
  (:message (first (get (:messages (get @mail id)) {:uid uid}))))

(defn copy-message [id uid to-id]
  (let [omsg (get-message uid)
        nm (MimeMessage. (.getMimeMessage omsg))
        nflags (Flags.)]
    (append-message to-id
                    (.add nflags (.getFlags omsg))
                    (.getInternalDate omsg))))

(defn -get-child [id child-name]
  (first (for [child-id (:children (get @mail id))
               :when (.equalsIgnoreCase
                      ^String (:name (get @mail child-id))
                      child-name)]
           child-id)))

;; HiMF is a dummy oop shell around a relational + functional
;; implementation

(defrecord HiMF [id]
  MailFolder
  (getName [_]
    (:name (get @mail id)))
  (getFullName [folder]
    (get-full-name id))
  (getPermanentFlags [_]
    permament-flags)
  (getMessageCount [_]
    (get-message-count id))
  (getRecentCount [_ reset?]
    (recent-count id reset?))
  (getUidValidity [_]
    (:uid-validity (get @mail id)))
  (getFirstUnseen [_]
    (get-first-unseen id))
  (getUnseenCount [_]
    (get-unseen-count id))
  (isSelectable [_]
    (boolean (:selectable? (get @mail id))))
  (getUidNext [_]
    (:next-uid (get @mail id)))
  (appendMessage [folder message flags internal-date]
    (long (append-message id message flags internal-date)))
  (deleteAllMessages [_]
    (dosync
     (alter mail update-in [id :messages] empty)))
  (expunge [_]
    (throw (Exception.)))
  (addListener [_ listener]
    (dosync
     (alter mail update-in [id :listeners] conj listener)))
  (removeListener [_ listener]
    (dosync
     (alter mail update-in [id :listeners] (partial remove (partial = listener)))))
  (^void store [folder ^com.icegreen.greenmail.mail.MovingMessage mail]
    (.store folder (.getMessage mail)))
  (^void store [folder ^javax.mail.internet.MimeMessage mail]
    (.store folder mail (java.util.Date.)))
  (^void store [folder ^javax.mail.internet.MimeMessage mail ^java.util.Date internal-date]
    (.appendMessage folder mail (Flags.) internal-date))
  (getMessageUids [_]
    (into-array Long/TYPE (map #(.getUid ^SimpleStoredMessage (:message %)) (:all (:messages (get @mail id))))))
  (getMessage [_ uid]
    (get-message id uid))
  (search [_ search-term]
    (into-array Long/TYPE
                (for [{:keys [message uid]} (:all (:messages (get @mail id)))
                      :when (.match search-term message)]
                  uid)))
  (copyMessage [_ uid to-folder]
    (copy-message id uid (:id to-folder)))
  (setFlags [_ flags value? uid listener add-uid?]
    (set-flags id flags value? uid listener add-uid?))
  (replaceFlags [_ flags uid silent-listener add-uid?]
    (replace-flags id flags uid silent-listener add-uid?))
  (getMsn [_ uid]
    (get-msn id uid))
  (signalDeletion [_]
    (signal-deletion id))
  (getMessages [_]
    (let [s (map :message (sort-by :msn (:all (:messages (get @mail id)))))]
      (java.util.ArrayList. ^java.util.Collection s)))
  (getMessages [_ range-filter]
    (let [s (for [{:keys [msn message]} (sort-by :msn (:all (:messages (get @mail id))))
                  :when (.includes range-filter msn)]
              message)]
      (java.util.ArrayList. ^java.util.Collection s)))
  (getNonDeletedMessages [_]
    (let [s (for [{:keys [^SimpleStoredMessage message]} (:all (:messages (get @mail id)))
                  :when (not (.contains (.getFlags message) Flags$Flag/DELETED))]
              message)]
      (java.util.ArrayList. ^java.util.Collection s)))
  HasChildren
  (get-children [_]
    (map ->HiMF (:children (get @mail id))))
  (get-child [_ child-name]
    (->HiMF (-get-child id child-name)))
  (add-child [_ child]
    (dosync
     (alter mail update-in [id :children] conj (:id child))))
  (remove-child [_ child]
    (dosync
     (alter mail update-in [id :children] disj (:id child))))
  HasParent
  (get-parent [_]
    (when-let [parent-id (:parent (get @mail id))]
      (->HiMF parent-id)))
  Nameable
  (set-name [_ new-name]
    (dosync
     (alter mail update-in [id] assoc :name new-name)))
  SelectEnabling
  (set-selectable [_ v]
    (dosync
     (alter mail update-in [id] assoc :selectable? (boolean v)))))

(defn mail-folder [parent name & [root?]]
  (let [id (UUID/randomUUID)]
    (dosync
     (alter mail assoc id {:parent (:id parent)
                           :name name
                           :uid-validity 0
                           :next-uid 1
                           :messages {}
                           :children #{}
                           :root? (boolean root?)}))
    (->HiMF id)))

(defn get-mailbox
  ([store absolute-name]
     (let [tokens (StringTokenizer. absolute-name hierarchy-delimiter-char)]
       (if (or (not (.hasMoreTokens tokens))
               (not (-> tokens .nextToken (.equalsIgnoreCase user-namespace))))
         nil
         (loop [parent-id (:id (.rootMailbox store))]
           (if (and (not (nil? parent-id))
                    (.hasMoreTokens tokens))
             (recur (-get-child parent-id (.nextToken tokens)))
             parent-id)))))
  ([store parent name]
     (-get-child (:id parent) name)))

(defn create-mailbox [parent mailbox-name selectable?]
  (when (not (= -1 (.indexOf mailbox-name hierarchy-delimiter-char)))
    (throw (FolderException. "Invalid mailbox name.")))
  (let [child (mail-folder parent mailbox-name)]
    (add-child parent child)
    (set-selectable child selectable?)
    child))
