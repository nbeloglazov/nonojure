(ns nn
  (:require [enclog
             [nnets :as nn]
             [training :as tr]
             [util :as ut]]))

(use 'clojure.repl)

(def net
  (nn/network (nn/neural-pattern :feed-forward)
              :activation :sigmoid
              :input   2
              :output  1
              :hidden [2]))


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

#_(def n (xor true))

#_(ut/eg-persist n "tmp.tmp")

#_(ut/eg-load "tmp.tmp")

#_(.compute n (tr/data :basic [0.0 0.0]))



