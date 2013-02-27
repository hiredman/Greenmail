(ns foo
  (:require [clojure.test :refer :all]))

(deftest foo
  (org.junit.runner.JUnitCore/main
   (into-array String
               ["com.icegreen.greenmail.CatchAllTest"
                "com.icegreen.greenmail.GreenMailUtilTest"
                "com.icegreen.greenmail.ImapServerTest"
                "com.icegreen.greenmail.SmtpServerTest"
                "com.icegreen.greenmail.MultiRequestTest"
                "com.icegreen.greenmail.store.SimpleMessageAttributesTest"
                "com.icegreen.greenmail.Pop3ServerTest"])))
