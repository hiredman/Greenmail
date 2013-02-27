(ns greenmail.store
  (:import (javax.mail Flags$Flag
                       Flags)
           (com.icegreen.greenmail.store MailFolder)
           (java.util UUID)))

(defn expunge [folder expunge-fun]
  (while (not= (count (.getMessageUids folder))
               (count (.getNonDeletedMessages folder)))
    (doseq [uid (.getMessageUids folder)
            :let [m (.getMessage folder uid)]
            :when (.contains (.getFlags m) Flags$Flag/DELETED)]
      (expunge-fun uid))))

(def mail (ref {}))

(defprotocol HasChildren
  (get-children [_])
  (get-child [_ child-id]))

(defprotocol HasParent
  (get-parent [_]))

(defprotocol Nameable
  (set-name [_ new-name]))

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
  (str (.getFullName (get-parent (->HiMF id)))
       hierarchy-delimiter-char
       (.getName (->HiMF id))))

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
    (dosync
     (ensure mail)
     (doseq [listener (:listeners (get @mail id))]
       (send-off a (fn [_] (.mailboxDeleted listener)))))))

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
  (appendMessage [_ message flags internal-date])
  (deleteAllMessages [_])
  (expunge [_])
  (addListener [_ listener])
  (removeListener [_ listener])
  (^void store [_ ^com.icegreen.greenmail.mail.MovingMessage mail])
  (^void store [_ ^javax.mail.internet.MimeMessage mail])
  (^void store [_ ^javax.mail.internet.MimeMessage mail
                ^java.util.Date internal-date])
  (getMessageUids [_])
  (search [_ search-term])
  (copyMessage [_ uid to-folder])
  (setFlags [_ flags value? uid listener add-uid?])
  (replaceFlags [_ flags uid listener add-uid?])
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
  HasParent
  (get-parent [_]
    (when-let [parent-id (:parent (get @mail id))]
      (->HiMF parent-id)))
  Nameable
  (set-name [_ new-name]
    (dosync
     (alter mail update-in [id] assoc :name new-name))))


(defn hier-mail-folder [parent name]
  (let [id (UUID/randomUUID)]
    (dosync
     (alter mail assoc id {:parent parent
                           :name name
                           :uid-validity 0
                           :next-uid 1
                           :messages []}))
    (->HiMF id)))
