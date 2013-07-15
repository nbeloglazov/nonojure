(defproject recognition "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [net.imagej/ij "1.47u"]
                 [incanter/incanter-charts "1.5.1"]
                 [org.tcrawley/dynapath "0.2.3"]
                 [com.cemerick/pomegranate "0.2.0"]
                 [clj-kdtree "1.1.0"
                  :exclusions [[org.clojure/clojure]]]
                 [org.opencv/opencv "2.4.6"]
                 [hu.kazocsaba/image-viewer "1.2.3"]]
  :repositories [["imagej.releases" "http://maven.imagej.net/content/repositories/releases"]]
  :resource-paths ["resources"]
  :jvm-opts ["-Djava.library.path=resources/native/"]
  :plugins [[lein-localrepo "0.5.2"]])
