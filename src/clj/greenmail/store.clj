(ns greenmail.store
  (:require [clojure.set :as set])
  (:import (javax.mail Flags$Flag
                       Flags)
           (javax.mail.internet MimeMessage)
           (com.icegreen.greenmail.store MailFolder
                                         SimpleStoredMessage
                                         MessageFlags)
           (java.util UUID)))

(def mail (ref {}))

(defn clear-mail []
  (dosync
   (ref-set mail {})))

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

(def permament-flags
  (doto (Flags.)
    (.add Flags$Flag/ANSWERED)
    (.add Flags$Flag/DELETED)
    (.add Flags$Flag/DRAFT)
    (.add Flags$Flag/FLAGGED)
    (.add Flags$Flag/SEEN)))

(declare ->HiMF)

(defn get-full-name [id]
  ;; TODO: get rid of calls to ->HiMF here
  (when (not (instance? UUID id))
    (.printStackTrace (Exception. "dumb")))
  (if (:root? (get @mail id))
    (:name (get @mail id))
    (str (get-full-name (:parent (get @mail id)))
         hierarchy-delimiter-char
         (:name (get @mail id)))))

(defn get-message-count [id]
  (count (:messages (get @mail id))))

(defn recent-count [id reset?]
  (apply + (for [message (:messages (get @mail id))
                 :when (.contains (.getFlags message) Flags$Flag/RECENT)]
             (do
               (when reset?
                 (.remove (.getFlags message) Flags$Flag/RECENT))
               1))))

(defn get-first-unseen [id]
  (or (first (for [[idx message] (map vector (range) (:messages (get @mail id)))
                   :when (not (.contains (.getFlags message) Flags$Flag/SEEN))]
               (inc idx)))
      -1))

(defn get-unseen-count [id]
  (apply + (for [message (:messages (get @mail id))
                 :when (not (.contains (.getFlags message) Flags$Flag/SEEN))]
             1)))

(defn get-msn [id uid]
  (first
   (for [[idx message] (map vector (range) (:messages (get @mail id)))
         :when (= (.getUid message) uid)]
     (inc idx))))

(defn signal-deletion [id]
  (let [a (agent nil)]
    (set-error-handler! a (fn [_ e]
                            (.printStackTrace e)))
    (dosync
     (ensure mail)
     (doseq [listener (:listeners (get @mail id))]
       (send-off a (fn [_] (.mailboxDeleted listener)))))
    (await a)))

(defn append-message [id message flags internal-date]
  (let [a (agent nil)
        _ (set-error-handler! a (fn [_ e]
                                  (.printStackTrace e)))
        uid (dosync
             (let [uid (:next-uid (get @mail id))
                   smsg (SimpleStoredMessage. message flags internal-date uid)]
               (alter mail update-in [id :next-uid] inc)
               (.add (.getFlags smsg) Flags$Flag/RECENT)
               (alter mail update-in [id :messages] conj smsg)
               (let [i (count (:messages (get @mail id)))]
                 (doseq [listener (:listeners (get @mail id))]
                   (send-off a (fn [_] (.added listener i)))))
               uid))]
    (await a)
    uid))

(defn set-flags [id flags value? uid silent-listener add-uid?]
  (let [msn (get-msn id uid)
        message (nth (:messages (get @mail id)) (dec msn))]
    (if value?
      (.add (.getFlags message) flags)
      (.remove (.getFlags message) flags))
    (let [a (agent nil)
          flags (.getFlags message)]
      (set-error-handler! a (fn [_ e]
                              (.printStackTrace e)))
      (dosync
       (ensure mail)
       (doseq [listener (:listeners (get @mail id))
               :when (not= listener silent-listener)]
         (send-off a (fn [_] (.flagsUpdated listener msn flags (when add-uid? uid))))))
      (await a))))

(defn replace-flags [id flags uid silent-listener add-uid?]
  (let [msn (get-msn id uid)
        message (nth (:messages (get @mail id)) (dec msn))]
    (.remove (.getFlags message) MessageFlags/ALL_FLAGS)
    (.add (.getFlags message) flags)
    (let [a (agent nil)
          flags (.getFlags message)]
      (set-error-handler! a (fn [_ e]
                              (.printStackTrace e)))
      (dosync
       (ensure mail)
       (doseq [listener (:listeners (get @mail id))
               :when (not= listener silent-listener)]
         (send-off a (fn [_] (.flagsUpdated listener msn flags (when add-uid? uid))))))
      (await a))))

(defn expunge [id]
  (let [a (agent nil)
        kept (ref [])
        expunged (ref [])]
    (set-error-handler! a (fn [_ e]
                            (.printStackTrace e)))
    (dosync
     (doseq [[idx message] (keep-indexed vector (:messages (get @mail id)))]
       (if (.contains (.getFlags message) Flags$Flag/DELETED)
         (alter expunged conj (inc idx))
         (alter kept conj message)))
     (alter mail update-in [id] assoc :messages @kept)
     (doseq [listener (:listeners (get @mail id))
             msn @expunged]
       (send-off a (fn [_] (.expunged listener msn)))))
    (await a)))

(defn get-message [id uid]
  (first (for [message (:messages (get @mail id))
               :when (= uid (.getUid message))]
           message)))

(defn copy-message [id uid to-id]
  (let [omsg (get-message uid)
        nm (MimeMessage. (.getMimeMessage omsg))
        nflags (Flags.)]
    (append-message to-id
                    (.add nflags (.getFlags omsg))
                    (.getInternalDate omsg))))

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
    (into-array Long/TYPE (map #(.getUid %) (:messages (get @mail id)))))
  (getMessage [_ uid]
    (get-message id uid))
  (search [_ search-term]
    (into-array Long/TYPE
                (for [message (:messages (get @mail id))
                      :when (.match search-term message)]
                  (.getUid message))))
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
    (java.util.ArrayList. (:messages (get @mail id))))
  (getMessages [_ range-filter]
    (java.util.ArrayList.
     (keep-indexed
      (fn [idx message]
        (when (.includes range-filter (inc idx))
          message))
      (:messages (get @mail id)))))
  (getNonDeletedMessages [_]
    (java.util.ArrayList.
     (for [message (:messages (get @mail id))
           :when (not (.contains (.getFlags message) Flags$Flag/DELETED))]
       message)))
  HasChildren
  (get-children [_]
    (map ->HiMF (:children (get @mail id))))
  (get-child [folder child-name]
    (first (for [child (get-children folder)
                 :when (.equalsIgnoreCase (.getName child) child-name)]
             child)))
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
                           :messages []
                           :children #{}
                           :root? (boolean root?)}))
    (->HiMF id)))
