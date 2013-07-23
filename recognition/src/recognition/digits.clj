(ns recognition.digits
  (:require [recognition
             [core :refer :all]
             [utils :as u]])
  (:import [org.opencv.core Core Mat CvType Scalar]))

(def size 30)

(def center (quot size 2))

(defn clear-borders! [digit]
  (doseq [m (concat (map #(.row digit %) [0 1 2])
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


#_(

   

   (let [quad (-> strut :left (nth 0) (nth 1))
         dig (clear-borders! (u/quad-to-rect orig quad [size size]))
         [left right] (separate dig (find-separator dig))]
     (def dig dig)
     (def left left)
     (def right right))

   (u/show dig)

   (u/show right)

   )
