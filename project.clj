(defproject sonian/greenmail "1.3.4-SNAPSHOT"
  :prep-tasks ["javac"]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [javax.mail/mail "1.4.4"]
                 [org.slf4j/slf4j-api "1.6.4"]]
  :source-paths ["src/clj"]
  :java-source-paths ["src/main"]
  :profiles {:dev {:dependencies [[org.slf4j/slf4j-simple "1.3.1"]
                                  [org.easytesting/fest-assert "1.4"]
                                  [junit "4.8.2"]]}})
