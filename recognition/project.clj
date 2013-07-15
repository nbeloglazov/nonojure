(defproject recognition "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [incanter/incanter-charts "1.5.1"]
                 [clj-kdtree "1.1.0"
                  :exclusions [[org.clojure/clojure]]]
                 [org.opencv/opencv "2.4.6"]
                 [hu.kazocsaba/image-viewer "1.2.3"]]
  :resource-paths ["resources"]
  :jvm-opts ["-Djava.library.path=resources/native/"]
  :plugins [[lein-localrepo "0.5.2"]])
