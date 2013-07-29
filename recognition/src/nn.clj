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

(xor false)

(defn input [name]
  (let [m (u/read (str "train-set/" name) "")]
    (.convertTo m m CvType/CV_32F (/ 1.0 255))
    (-> (.reshape m 1 1)
        (MatOfFloat.)
        (.toList))))

(defn output [name]
  (assoc [0 0 0 0 0 0 0 0 0 0] (- (int (first name)) (int \0)) 1))

(defn train-dataset []
  (->> (io/file "train-set")
       (file-seq)
       (filter #(.isFile %))
       (map #(.getName %))
       (map #(vector (input %) (output %)))
       (apply map vector)
       (apply tr/data :basic-dataset)))


(def net
  (nn/network (nn/neural-pattern :feed-forward)
              :activation :sigmoid
              :input   900
              :output  10
              :hidden [700]))

(def file "1.nono4_up_2_0.png")

(def dataset (train-dataset))

(def trainer (tr/trainer :resilient-prop :network net :training-set dataset))

(def test (tr/data :basic (input file)))

#_(tr/train trainer 0.00000001 1000 [])

#_(ut/eg-persist net "network.eg")

#_(seq (.getData (.compute net test)))



