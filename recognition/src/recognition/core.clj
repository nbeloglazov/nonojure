(ns recognition.core
  (:import [ij IJ ImagePlus]
           [ij.process ImageProcessor ByteProcessor]
           [ij.plugin.filter MaximumFinder]))

(defn read-image [file]
  (->> file
       (str "resources/")
       java.io.File.
       javax.imageio.ImageIO/read
       (ImagePlus. file)))


(let [maximum-finder (MaximumFinder.)]
  (defn find-maxima
    "Findx local maxima of image and return instance of ByteProcessor where 255 - max. 0 - otherwise."
    [image]
    (let [tolerance 2.0
          threshold ImageProcessor/NO_THRESHOLD
          output-type MaximumFinder/SINGLE_POINTS
          orig-proc (-> image .getProcessor .convertToFloat)]
      (.findMaxima maximum-finder orig-proc tolerance threshold output-type true true))))


(defn find-cells-candidates [^ImagePlus image]
  (let [dup (doto (.duplicate image)
                   (IJ/run "Invert" "")
                   (IJ/run "Distance Map" ""))
         ^ByteProcessor maxima (find-maxima dup)]
    maxima
    #_(for [^int y (range (.getHeight maxima))
          ^int x (range (.getWidth maxima))
          :when (= (.get maxima x y) 255)]
      [x y])))

(defn get-image [name]
  (doto (read-image name)
    (IJ/run "Threshold" "")))

(def orig (get-image "nono1.png"))


(.show orig)



(.show (ImagePlus. "sdfsdf" (find-cells-candidates (get-image "nono2.png"))))

