(ns nn
  (:require [enclog
             [nnets :as nn]
             [training :as tr]
             [util :as ut]]
            [recognition
             [utils :as u]]
            [clojure.java.io :as io])
  (:import [org.opencv.core Mat MatOfByte MatOfInt CvType MatOfFloat]))

(use 'clojure.repl)

(defn xor [train-to-error?]
  (let [xor-input [[0.0 0.0] [1.0 0.0] [0.0 0.1] [1.0 1.0]]
        xor-ideal [[0.0] [1.0] [1.0] [0.0]]
        dataset (tr/data :basic-dataset xor-input xor-ideal)
        net (nn/network (nn/neural-pattern :feed-forward)
                        :activation :sigmoid
                        :input 2
                        :output 1
                        :hidden [2]) ;a single hidden layer
        trainer (tr/trainer :resilient-prop :network net :training-set dataset)]
    ;;make use of the boolean parameter
    (if train-to-error?
      (tr/train trainer 0.01 []) ;train to max error regardless of iterations
      (tr/train trainer 0.01 300 [])) ;;train to max iterations and max error
    (do
      (println "\nNeural Network Results:")
      net)))

(defn input [dir name]
  (let [m (u/read (str dir "/" name) "")]
    (.convertTo m m CvType/CV_32F (/ 1.0 255))
    (-> (.reshape m 1 1)
        (MatOfFloat.)
        (.toList))))

(defn output [name]
  (assoc [0 0 0 0 0 0 0 0 0 0] (- (int (first name)) (int \0)) 1))

(defn read-dataset [dir]
  (->> (io/file dir)
       (file-seq)
       (filter #(.isFile %))
       (map #(.getName %))
       (map #(vector (input dir %) (output %)))
       (apply map vector)
       (apply tr/data :basic-dataset)))


(def net
  (nn/network (nn/neural-pattern :feed-forward)
              :activation :sigmoid
              :input   900
              :output  10
              :hidden [700]))

(def dataset (read-dataset "train-set"))

(def test (read-dataset "test-set"))

(def trainer (tr/trainer :resilient-prop :network net :training-set dataset))

#_(

   (tr/train trainer 0.001 1000 [])

   (ut/eg-persist net "network.eg")

   (seq (.getData (.compute net test)))

   (def ex (.getInput (first (.getData test))))

   (seq (.getData (.compute net ex)))

   (doseq [pair (.getData test)
           :let [input (.getInput pair)
                 ideal (.getIdealArray pair)]]
     (->> (.compute net input)
          (.getData)
          seq
          (map - (seq ideal))
          (map #(Math/abs %))
          (reduce +)
          println))

   )
