(ns greenmail.pop3
  (:import (com.icegreen.greenmail.pop3.commands Pop3Command
                                                 Pop3CommandRegistry)
           (com.icegreen.greenmail.store FolderException)))

(def quit-is-valid? (constantly true))

(defn quit-execute [conn state cmd]
  (try
    (when-let [folder (.getFolder state)]
      (while (not= (.getMessageCount folder)
                   (count (.getNonDeletedMessages folder)))
        (.expunge folder)))
    (.println conn "+OK by see you soon")
    (.quit conn)
    (catch FolderException e
      (.println conn "+OK signing off, but message deletion failed")
      (.quit conn))))

(defn sum-message-sizes [messages]
  (apply + (map #(.getSize (.getMimeMessage %)) messages)))

(defn stat-is-valid? [state]
  (.isAuthenticated state))

(defn stat-execute [conn state cmd]
  (try
    (let [folder (.getFolder state)
          messages (.getNonDeletedMessages folder)]
      (.println conn
                (str "+OK " (count messages) " " (sum-message-sizes messages))))
    (catch Exception e
      (.println conn (str "-ERR " e)))))

(Pop3CommandRegistry/registerCommand
 "QUIT"
 (reify
   Pop3Command
   (isValidForState [_ state] (boolean (quit-is-valid?)))
   (execute [_ conn state cmd] (quit-execute conn state cmd))))

(Pop3CommandRegistry/registerCommand
 "STAT"
 (reify
   Pop3Command
   (isValidForState [_ state] (boolean (stat-is-valid? state)))
   (execute [_ conn state cmd] (stat-execute conn state cmd))))
