(ns recognition.morphology
  (:import org.opencv.imgproc.Imgproc
           [org.opencv.core Mat Size CvType Scalar Core]))

(recognition.Loader/loadLibrary "opencv_java246")

(defn struct-element [type [width height]]
  (let [type (case type
               :rect Imgproc/MORPH_RECT
               :ellipse Imgproc/MORPH_ELLIPSE
               :cross Imgproc/MORPH_CROSS
               (throw (IllegalArgumentException. (str "Unsupported struct element type " type))))]
      (Imgproc/getStructuringElement type (Size. width height))))

(defn dilate! [mat el]
  (Imgproc/dilate mat mat el)
  mat)

(defn erode! [mat el]
  (Imgproc/erode mat mat el)
  mat)

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

(defn skeleton [mat el]
  (let [mat (.clone mat)
        skel (Mat. (.size mat) CvType/CV_8UC1 (Scalar. 0.0))
        temp (Mat. (.size mat) CvType/CV_8UC1)
        eroded (Mat. (.size mat) CvType/CV_8UC1)]
    (while (not (zero? (Core/countNonZero mat)))
      (Imgproc/erode mat eroded el)
      (Imgproc/dilate eroded temp el)
      (Core/subtract mat temp temp)
      (Core/bitwise_or skel temp skel)
      (.copyTo eroded mat))
    skel))

#_(do
    (require '[recognition.opencv :as c])
    (-> "nono5.jpg"
        c/read
        c/adaptive-threshold!
        c/invert!
        (skeleton (struct-element :ellipse [3 3]))
        c/invert!
        c/show)
)


