(ns recognition.opencv
  (:import [java.awt.image BufferedImage]
           [org.opencv.core Mat MatOfByte CvType Scalar Core]
           org.opencv.highgui.Highgui
           javax.imageio.ImageIO
           org.opencv.imgproc.Imgproc))

(recognition.Loader/loadLibrary "opencv_java246")

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
            (let [viewer (hu.kazocsaba.imageviewer.ImageViewer. im)
                  fr (javax.swing.JFrame. "Image")]
              (.setStatusBarVisible viewer true)
              (.setPixelatedZoom viewer true)
              (adapt-size fr mat)
              (.setLocationRelativeTo fr nil)
              (.. fr getContentPane (add (.getComponent viewer)))
              (.setVisible fr true)
              mat))]
    (show-image (to-image mat))))

(defn read [file]
  (let [m (Highgui/imread (str "resources/examples/" file))]
    (Imgproc/cvtColor m m Imgproc/COLOR_RGB2GRAY)
    m))

(defn save [mat file]
  (Highgui/imwrite file mat))

(defn invert! [mat]
  (Core/bitwise_not mat mat)
  mat)

(defn adaptive-threshold! [mat]
  (let [new (.clone mat)]
    (Imgproc/adaptiveThreshold mat
                               mat
                               255.0 ; max value
                               Imgproc/ADAPTIVE_THRESH_MEAN_C
                               Imgproc/THRESH_BINARY
                               21 ; size
                               10 ; - C
                               )
    mat))

#_(->> "nono5.jpg" read adaptive-threshold! show)

#_(show (read "nono5.jpg"))

