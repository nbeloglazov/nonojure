(ns recognition.morphology
  (:require [recognition
             [classpath :as classpath]]))

(classpath/load-libs)

(import [org.ajdecon.morphology StructElement Morphology])

(defn struct-element
  "Type :circle or :square - size is an int
Type :ring, :rect, :line - size is an 2 element vector of ints."
  [type size & {:keys [bg-white?]
                :or {bg-white? false}}]
  (condp contains? type
    #{:circle :square} (StructElement. (name type) size bg-white?)
    #{:ring :rect :line} (StructElement. (name type) (first size) (second size) bg-white?)
    (throw (IllegalArgumentException. (str "type " type " is not supported")))))

(defmacro wrap-proc [body]
  `(-> (ij.ImagePlus. "temp" ~'proc)
       ~body
       .getProcessor))

(defn erode [proc s]
  (wrap-proc (Morphology/erode s)))

(defn dilate [proc s]
  (wrap-proc (Morphology/dilate s)))

(defn open [proc s]
  (wrap-proc (Morphology/open s)))

(defn close [proc s]
  (wrap-proc (Morphology/close s)))

#_(do
    (require '[recognition.core :as c])
    (def s (struct-element :square 21))
    (-> (c/duplicate c/orig)
        (open s)
        c/show)
    (open (c/duplicate c/orig) s)
    (c/show c/orig))


