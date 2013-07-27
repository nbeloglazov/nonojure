(ns recognition.digits
  (:require [recognition
             [core :as c]
             [utils :as u]
             [morphology :as mor]]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import [org.opencv.core Core Mat CvType Scalar]))

(def size 30)

(def center (quot size 2))

(defn clear-borders! [digit]
  (doseq [m (concat (map #(.row digit %) [0 1 2 (- size 2) (- size 1)])
                    (map #(.col digit %) [0 1 (- size 2) (- size 1)]))]
    (.setTo m (Scalar. 255.0)))
  digit)

(defn find-separator [digit]
  (let [weight (fn [n]
                 (-> (.col digit n) Core/sumElems .val (aget 0)))]
    (apply max-key (memoize weight) (range 5 (inc center)))))

(defn separate [digit col]
  (letfn [(subdigit [from to]
            (let [res (Mat. size size CvType/CV_8UC1 (Scalar. 255.0))
                  width (- to from)
                  dst-from (- center (quot width 2))
                  dst (.submat res 0 (dec size) dst-from (+ dst-from width))
                  src (.submat digit 0 (dec size) from to)]
              (.copyTo src dst)
              res))]
    [(subdigit 0 (dec col))
     (subdigit (inc col) (dec size))]))


(defn extract-digit-images [name]
  (let [orig (->> (str name ".jpg") u/read c/fit-to-1000! c/adaptive-threshold!)
        im (->> (.clone orig) u/invert! mor/skeleton c/remove-noise u/invert!)
        real-nono (->> (str "parsed/" name ".clj") io/resource slurp edn/read-string)
        nono (c/parse-structure im)
        dig-filename (fn [value part ind1 ind2]
                       (format "train-set/%d.%s_%s_%d_%d.png" value name (clojure.core/name part) ind1 ind2))
        process-part (fn [part]
                       (let [n-part (nono part)
                             rn-part (real-nono part)]
                         (doseq [ind1 (range (count n-part))
                                 :when (= (count (n-part ind1))
                                          (count (rn-part ind1)))
                                 ind2 (range (count (n-part ind1)))]
                           (let [value (get-in rn-part [ind1 ind2])]
                             (when (< value 10)
                               (u/save (clear-borders! (u/quad-to-rect orig (get-in n-part [ind1 ind2]) [size size]))
                                       (dig-filename value part ind1 ind2)))))))]
    (process-part :left)
    (process-part :up)))

#_(

   (doseq [ind (range 4 10)]
     (println ind)
     (extract-digit-images (str "nono" ind)))
   (extract-digit-images "nono4")

   (->> (io/file "train-set")
        (file-seq)
        (map #(.getName %))
        (map first)
        frequencies)

   (defn move-to-train [files]
     (doseq [file (take 25 (shuffle files))]
       (.renameTo file (io/file (str "train-set/" (.getName file))))))

   (->> (io/file "test-set")
        (file-seq)
        (group-by #(first (.getName %)))
        (filter (fn [[k v]]
                  (<= (int \1) (int k) (int \9))))
        (map second)
        (map move-to-train)
        (dorun))


   (let [quad (-> strut :left (nth 0) (nth 1))
         dig (clear-borders! (u/quad-to-rect orig quad [size size]))
         [left right] (separate dig (find-separator dig))]
     (def dig dig)
     (def left left)
     (def right right))

   (u/show dig)

   (u/show right)

   )
