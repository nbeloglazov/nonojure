(defproject recognition "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [incanter/incanter-charts "1.5.1"]
                 [clj-kdtree "1.2.0"
                  :exclusions [[org.clojure/clojure]]]
                 [org.opencv/opencv "2.4.6"]
                 [hu.kazocsaba/image-viewer "1.2.3"]
                 [org.encog/encog-core "3.1.0"]
                 [enclog "0.6.3"]
                 [seesaw "1.4.3"]
                 [net.sf/javaml "0.1.7"]]
  :repositories [["javaml" {:url "http://corp.array.ca/nest-web/maven/"
                            :checksum :warn}]]
  :resource-paths ["resources"]
  :jvm-opts ["-Djava.library.path=resources/native/"]
  :plugins [[lein-localrepo "0.5.2"]]
  :main recognition.gui)
