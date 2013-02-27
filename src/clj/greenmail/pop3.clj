(ns greenmail.pop3
  (:import (com.icegreen.greenmail.pop3.commands Pop3Command
                                                 Pop3CommandRegistry)
           (com.icegreen.greenmail.store FolderException)
           (com.icegreen.greenmail.util GreenMailUtil)
           (com.icegreen.greenmail.foedus.util MsgRangeFilter)
           (javax.mail Flags
                       Flags$Flag)
           (java.io StringReader)))

(def commands (atom {}))

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

(defn authenticated? [state]
  (.isAuthenticated state))

(def not-authenticated? (complement authenticated?))

(defn stat-execute [conn state cmd]
  (try
    (let [folder (.getFolder state)
          messages (.getNonDeletedMessages folder)]
      (.println conn
                (str "+OK " (count messages) " " (sum-message-sizes messages))))
    (catch Exception e
      (.println conn (str "-ERR " e)))))

(defn dele-execute [conn state cmd]
  (try
    (let [inbox (.getFolder state)
          [_ msg-number] (.split cmd " ")
          messages (.getMessages inbox (MsgRangeFilter. msg-number false))]
      (if (not= 1 (count messages))
        (.println conn "-ERR no such message")
        (let [[msg] (seq messages)
              flags (.getFlags msg)]
          (if (.contains flags Flags$Flag/DELETED)
            (.println conn "-ERR message already deleted")
            (do
              (.add flags Flags$Flag/DELETED)
              (.println conn "+OK message scheduled for deletion"))))))
    (catch Exception e
      (.println conn (str "-ERR " e)))))

(defn apop-execute [conn state cmd]
  (.println conn "-ERR APOP not supported"))

(defn noop-execute [conn state cmd]
  (.println conn "+OK noop is balls"))

(defn retr-execute [conn state cmd]
  (try
    (let [inbox (.getFolder state)
          [_ msg-number] (.split cmd " ")
          messages (.getMessages inbox (MsgRangeFilter. msg-number false))]
      (if (not= 1 (count messages))
        (.println conn "-ERR no such message")
        (let [[msg] (seq messages)
              email (GreenMailUtil/getWholeMessage (.getMimeMessage msg))]
          (doto conn
            (.println "+OK")
            (.print (StringReader. email))
            (.println)
            (.println "."))
          (-> msg .getFlags (.add Flags$Flag/SEEN)))))
    (catch Exception e
      (.println conn (str "-ERR " e)))))

(defn uidl-execute [conn state cmd]
  (try
    (let [inbox (.getFolder state)
          cmd-line (.split cmd " ")]
      (if (> (count cmd-line) 1)
        (let [[_ msg-number] cmd-line
              messages (.getMessages inbox (MsgRangeFilter. msg-number false))]
          (if (not= 1 (count messages))
            (.println conn "-ERR no such message")
            (.println conn (str "+OK " msg-number " " (.getUid (first messages))))))
        (let [messages (.getNonDeletedMessages inbox)]
          (.println conn "+OK")
          (doseq [message messages]
            (.println conn (str (.getMsn inbox (.getUid message)) " "
                                (.getUid message))))
          (.println conn "."))))
    (catch FolderException e
      (.println conn (str "-ERR " e)))))

(defn register [& {:keys [name validator executor]}]
  (swap! commands assoc (.toUpperCase name)
         (reify
           Pop3Command
           (isValidForState [_ state] (boolean (validator state)))
           (execute [_ conn state cmd] (executor conn state cmd)))))

(register
 :name "quit"
 :validator (constantly true)
 :executor quit-execute)

(register
 :name "stat"
 :validator authenticated?
 :executor stat-execute)

(register
 :name "dele"
 :validator authenticated?
 :executor dele-execute)

(register
 :name "apop"
 :validator not-authenticated?
 :executor apop-execute)

(register
 :name "noop"
 :validator (constantly true)
 :executor noop-execute)

(register
 :name "retr"
 :validator authenticated?
 :executor retr-execute)

(register
 :name "uidl"
 :validator authenticated?
 :executor uidl-execute)

(doseq [[command klass]
        {"USER" com.icegreen.greenmail.pop3.commands.UserCommand
         "PASS" com.icegreen.greenmail.pop3.commands.PassCommand
         "LIST" com.icegreen.greenmail.pop3.commands.ListCommand
         "TOP" com.icegreen.greenmail.pop3.commands.TopCommand
         "RSET" com.icegreen.greenmail.pop3.commands.RsetCommand}]
  (swap! commands assoc command (.newInstance klass)))
