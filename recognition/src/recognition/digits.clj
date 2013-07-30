(ns recognition.digits
  (:require [recognition
             [core :as c]
             [utils :as u]
             [morphology :as mor]]
            [enclog
             [nnets :as nnets]
             [training :as train]
             [util :as en-ut]]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import [org.opencv.core Core Mat CvType Scalar MatOfFloat]))

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


(def net (en-ut/eg-load "network.eg"))

(defn mat-to-array [mat]
  (let [m (Mat.)]
   (.convertTo mat m CvType/CV_32F (/ 1.0 255))
   (-> (.reshape m 1 1)
      (MatOfFloat.)
      (.toList))))

(defn recognize-digit [dig]
  (let [score (fn [output]
                (let [mx (apply max output)]
                  (- (+ mx mx) (apply + output))))
        recognize (fn [dig]
                    (->> (mat-to-array dig)
                    (train/data :basic)
                    (.compute net)
                    .getData
                    seq))
        to-digit (fn [output]
                   (apply max-key second (map-indexed vector output)))
        single (recognize dig)
        [dbl-l dbl-r] (->> (find-separator dig)
                           (separate dig)
                           (map recognize))]
    (if (> (score single)
           (* (score dbl-l)
              (score dbl-r)))
      [(to-digit single)]
      (map to-digit [dbl-l dbl-r]))))

(defn recognize-all-digits [mat nono]
  (letfn [(extract [positions]
            (clear-borders! (u/quad-to-rect mat positions [size size])))
          (recognize-row [row]
            (map #(recognize-digit (extract %)) row))
          (recognize-part [part]
            (map recognize-row part))]
    (reduce #(update-in %1 [%2] recognize-part) nono [:left :up])))


;;; Creating train set

(defn extract-digit-images [name]
  (let [orig (->> (str name ".jpg") u/read c/fit-to-1000! c/adaptive-threshold!)
        im (->> (.clone orig) u/invert! mor/skeleton c/remove-noise u/invert!)
        real-nono (->> (str "parsed/" name ".clj") io/resource slurp edn/read-string)
        nono (c/parse-structure im)
        dig-filename (fn [value part ind1 ind2 other]
                       (format "test-set/%d.%s_%s_%d_%d%s.png" value name (clojure.core/name part) ind1 ind2 other))
        process-part (fn [part]
                       (let [n-part (nono part)
                             rn-part (real-nono part)]
                         (doseq [ind1 (range (count n-part))
                                 :when (= (count (n-part ind1))
                                          (count (rn-part ind1)))
                                 ind2 (range (count (n-part ind1)))]
                           (let [value (get-in rn-part [ind1 ind2])
                                 digit (clear-borders! (u/quad-to-rect orig (get-in n-part [ind1 ind2]) [size size]))]
                             (if (< value 10)
                               (u/save digit (dig-filename value part ind1 ind2 ""))
                               (let [[left right] (separate digit (find-separator digit))]
                                 (u/save left (dig-filename (quot value 10) part ind1 ind2 "_1of2"))
                                 (u/save right (dig-filename (mod value 10) part ind1 ind2 "_2of2"))))))))]
    (process-part :left)
    (process-part :up)))





;;; Training neural network

(defn input [dir name]
  (mat-to-array (u/read (str dir "/" name) "")))

(defn output [name]
  (assoc [0 0 0 0 0 0 0 0 0 0] (- (int (first name)) (int \0)) 1))

(defn read-dataset [dir]
  (->> (io/file dir)
       (file-seq)
       (filter #(.isFile %))
       (map #(.getName %))
       (map #(vector (input dir %) (output %)))
       (apply map vector)
       (apply train/data :basic-dataset)))



#_(

   (def net
     (nnet/network (nnet/neural-pattern :feed-forward)
                 :activation :sigmoid
                 :input   900
                 :output  10
                 :hidden [700]))

   (def dataset (read-dataset "train-set"))

   (def test (read-dataset "test-set"))

   (def trainer (train/trainer :resilient-prop :network net :training-set dataset))

   (train/train trainer 0.001 1000 [])

   (eg-ut/eg-persist net "network.eg")



   (en-ut/eg-load "")

   (doseq [ind (range 4 10)]
     (println ind)
     (extract-digit-images (str "nono" ind)))

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


   (def res (recognize-all-digits c/orig c/strut))

   (doseq [part [:left :up]]
     (println)
     (println part)
     (doseq [row (res part)]
       (println)
       (doseq [v row]
         (print " " (apply str (map first v))))))

   (def d6 (clear-borders! (u/quad-to-rect c/orig (-> c/strut :left (nth 0) first) [size size])))

   (def d14 (clear-borders! (u/quad-to-rect c/orig (-> c/strut :left (nth 5) first) [size size])))

   (def d11 (clear-borders! (u/quad-to-rect c/orig (-> c/strut :left (nth 6) first) [size size])))



   (u/show d11)

   (recognize-digit (first (separate d11 (find-separator d11))))

   (recognize-digit d11)



   )
