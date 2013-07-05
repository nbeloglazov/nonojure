(ns recognition.classpath
  (:require [dynapath.util :as dp]
            [clojure.java.io :as io]))


(defn- get-libs-from-dir [dir]
  (->> dir
       io/as-file
       file-seq
       (remove #(.isDirectory %))
       (map io/as-url)))

(defn- get-libs []
  (let [libs-dir (io/resource "libs")]
    (if (= (.getProtocol libs-dir) "file")
      (get-libs-from-dir libs-dir)
      (throw (IllegalArgumentException. "jar files are not supported yet")))))

(defn- classloader-hierarchy []
  (->> (.. Thread currentThread getContextClassLoader)
       (iterate #(.getParent %))
       (take-while boolean)))


(defn- modifiable-classloader []
  (->> (classloader-hierarchy)
       (filter dp/addable-classpath?)
       last))

(defn- add-to-classpath [libs]
  (when-let [cl (modifiable-classloader)]
    (doseq [lib libs]
      (dp/add-classpath-url cl lib))))


(defn load-libs []
  (add-to-classpath (get-libs)))

