(ns recognition.utils
  (:import [java.awt.image BufferedImage]
           [org.opencv.core Mat MatOfByte CvType Scalar Core Size Point MatOfPoint2f]
           [hu.kazocsaba.imageviewer ImageViewer ResizeStrategy]
           org.opencv.highgui.Highgui
           javax.imageio.ImageIO
           org.opencv.imgproc.Imgproc))

(clojure.lang.RT/loadLibrary "opencv_java246")

(defn show [mat]
  (letfn [(to-image [mat]
            (let [bytes (MatOfByte.)]
              (Highgui/imencode ".png" mat bytes)
              (-> (.toArray bytes)
                  (java.io.ByteArrayInputStream.)
                  (ImageIO/read))))
          (adapt-size [frame mat]
            (let [w (.width mat)
                  h (.height mat)
                  [s-w s-h] (->> (java.awt.Toolkit/getDefaultToolkit)
                                 .getScreenSize
                                 ((juxt #(.getWidth %) #(.getHeight %)))
                                 (map #(* 0.8 %)))
                  ratio (/ w h)
                  [m-w m-h] (map #(min %1 %2) [w h] [s-w s-h])]
              (if (> m-w (* ratio m-h))
                (.setSize frame (* ratio m-h) m-h)
                (.setSize frame m-w (/ m-w ratio)))))
          (show-image [im]
            (let [viewer (ImageViewer. im)
                  frame (javax.swing.JFrame. "Image")]
              (doto viewer
                (.setStatusBarVisible true)
                (.setPixelatedZoom true)
                (.setResizeStrategy ResizeStrategy/RESIZE_TO_FIT))
              (doto frame
                (adapt-size mat)
                (.setLocationRelativeTo nil)
                (..  getContentPane (add (.getComponent viewer)))
                (.setVisible true))
              mat))]
    (show-image (to-image mat))))

(defn to-binary-mat [colls]
  (-> (apply concat colls)
      (->>
       (map #(if (zero? %) 0 -1))
       (into-array Byte/TYPE))
      (MatOfByte.)
      (.reshape 1 (count colls))))

(defn to-vecs [mat]
  (for [i (range (.rows mat))]
    (for [j (range (.cols mat))]
      (-> (.get mat i j) seq first))))

(defn read [file]
  (let [m (Highgui/imread (str "resources/examples/" file))]
    (Imgproc/cvtColor m m Imgproc/COLOR_RGB2GRAY)
    m))

(defn save [mat file]
  (Highgui/imwrite file mat))

(defn invert [mat dst]
  (Core/bitwise_not mat dst)
  dst)

(defn invert! [mat]
  (invert mat mat))

(defn transpose [mat]
  (.t mat))

(defn flip! [mat dir]
  (Core/flip mat mat (case dir
                       :x 1
                       :y 0))
  mat)

(defn clone [mat]
  (.clone mat))

(defn subtract [src1 src2 dst]
  (Core/subtract src1 src2 dst)
  dst)

(defn rotate90 [mat]
  (flip! (transpose mat) :x))

(defn zero-mat? [mat]
  (zero? (Core/countNonZero mat)))

(defn resize! [mat scale]
  (Imgproc/resize mat mat (Size.) scale scale Imgproc/INTER_LINEAR)
  mat)

(defn mat-or [src1 src2 dst]
  (Core/bitwise_or src1 src2 dst)
  dst)

(defn put-text! [mat text [x y]]
  (Core/putText mat text (Point. x y) Core/FONT_HERSHEY_SIMPLEX 0.25 (Scalar. 0.0))
  mat)

(defn draw-square! [mat [x y] size]
  (let [hs (* 0.5 size)]
    (Core/rectangle mat (Point. (- x hs) (- y hs))
                    (Point. (+ x hs) (+ y hs))
                    (Scalar. 0.0))
    mat))

(defn draw-circle! [mat [x y] size]
  (Core/circle mat (Point. x y) (* 0.5 size) (Scalar. 0.0))
  mat)

(defn draw-line! [mat [x0 y0] [x1 y1]]
  (Core/line mat (Point. x0 y0) (Point. x1 y1) (Scalar. 0.0))
  mat)

(defn majority [vals]
  (->> (frequencies vals)
       (sort-by last)
       last
       first))

(defn mat-of-points [points]
  (let [m (MatOfPoint2f.)
        point (fn [[x y]] (Point. x y))]
    (.fromList m (map point points))
    m))

(defn perspective-transform [from to]
  (let [from (mat-of-points from)
        to (mat-of-points to)]
    (Imgproc/getPerspectiveTransform from to)))

(defn quad-to-rect [mat quad [rect-width rect-height]]
  (let [transf (perspective-transform quad
                                      [[0 0] [rect-width 0] [rect-width rect-height] [0 rect-height]])
        res (Mat. rect-height rect-width CvType/CV_8UC1)]
    (Imgproc/warpPerspective mat res transf (Size. rect-width rect-height) Imgproc/INTER_NEAREST)
    res))
