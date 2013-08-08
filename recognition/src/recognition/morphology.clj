(ns recognition.morphology
  (:import org.opencv.imgproc.Imgproc
           [org.opencv.core Mat Size CvType Scalar Core])
  (:require [recognition
             [utils :as u]
             [trace :refer [with-scope]]]))

(recognition.Loader/loadLibrary "opencv_java246")

(defn struct-element [type [width height]]
  (let [type (case type
               :rect Imgproc/MORPH_RECT
               :ellipse Imgproc/MORPH_ELLIPSE
               :cross Imgproc/MORPH_CROSS
               (throw (IllegalArgumentException. (str "Unsupported struct element type " type))))]
      (Imgproc/getStructuringElement type (Size. width height))))

(defn hom-mask [mask]
  (letfn [(filter [v]
            (map (partial map #(if (= % v) 1 0)) mask))]
    (->> [1 -1]
         (map filter)
         (map u/to-binary-mat))))

(defn dilate! [mat el]
  (Imgproc/dilate mat mat el)
  mat)

(defn erode [mat dst el]
  (Imgproc/erode mat dst el)
  dst)

(defn erode! [mat el]
  (erode mat mat el))

(defn- morphology-ex [mat op el]
  (Imgproc/morphologyEx mat mat op el)
  mat)

(defn open! [mat el]
  (morphology-ex mat Imgproc/MORPH_OPEN el))

(defn close! [mat el]
  (morphology-ex mat Imgproc/MORPH_CLOSE el))

(defn top-hat! [mat el]
  (morphology-ex mat Imgproc/MORPH_TOPHAT el))

(defn black-hat! [mat el]
  (morphology-ex mat Imgproc/MORPH_BLACKHAT el))

(defn hit-or-miss [mat [a b]]
  (let [e1 (Mat.)
        e2 (Mat.)]
    (erode mat e1 a)
    (erode! (u/invert mat e2) b)
    (Core/bitwise_and e1 e2 e2)
    e2))

(defn thinning [mat mask]
  (let [hom  (hit-or-miss mat mask)]
    (Core/subtract mat hom hom)
    hom))

(defn skeleton2 [mat el]
  (let [mat (.clone mat)
        skel (Mat. (.size mat) CvType/CV_8UC1 (Scalar. 0.0))
        temp (Mat. (.size mat) CvType/CV_8UC1)
        eroded (Mat. (.size mat) CvType/CV_8UC1)]
    (while (not (u/zero-mat? mat))
      (Imgproc/erode mat eroded el)
      (Imgproc/dilate eroded temp el)
      (Core/subtract mat temp temp)
      (Core/bitwise_or skel temp skel)
      (.copyTo eroded mat))
    skel))

(def ^:private skeleton-patterns
  (letfn [(parse-and-rotate [mask]
            (->> (hom-mask mask)
                 (iterate (partial map u/rotate90))
                 (take 4)))]
    (mapcat parse-and-rotate
            [[[ 1  1  1]
              [ 0  1  0]
              [-1 -1 -1]]
             [[ 0  1  0]
              [-1  1  1]
              [-1 -1  0]]])))

(defn skeleton [mat]
  (with-scope :skeletonization
    (let [iteration (fn [mat]
                      (reduce thinning mat skeleton-patterns))]
      (loop [cur (iteration mat)
             prev (u/clone mat)
             it 0]
        (if (u/zero-mat? (u/subtract prev cur prev))
          cur
          (recur (iteration cur)
                 cur
                 (inc it)))))))
