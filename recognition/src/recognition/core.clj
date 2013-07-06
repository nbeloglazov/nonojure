(ns recognition.core
  (:import [ij IJ ImagePlus]
           [ij.process ImageProcessor ByteProcessor ByteStatistics ImageConverter]
           [ij.plugin.filter MaximumFinder GaussianBlur EDM])
  (:require [incanter
             [core :as core]
             [charts :as charts]]
            [clojure.java.io :as io]
            [recognition
             [classpath :as classpath]]))

(classpath/load-libs)

(defn processor [image]
  (.getProcessor image))

(defn image [processor]
  (ImagePlus. "image" processor))

(defn show [processor]
  (.show (image processor)))

(defn statistics [processor]
  (.getStatistics processor))

(defn histogram [processor]
  (vec (.getHistogram (statistics processor))))

(defn duplicate [processor]
  (.duplicate processor))

(defn save [processor path]
  (-> (image processor)
      ij.io.FileSaver.
      (.saveAsPng path)))

(defn crop [processor x y width height]
  (.setRoi processor x y width height)
  (.crop processor))

(defn center-third [processor]
  (let [w (.getWidth processor)
        h (.getHeight processor)]
    (crop processor (/ w 3) (/ h 3) (/ w 3) (/ h 3))))

(defn find-maxima-on-image
  "Findx local maxima of image and return instance of ByteProcessor where 255 - max. 0 - otherwise."
  [processor]
  (let [maximum-finder (MaximumFinder.)
        tolerance 2.0
        threshold ImageProcessor/NO_THRESHOLD
        output-type MaximumFinder/SINGLE_POINTS
        orig-proc (.convertToFloat processor)]
    (.findMaxima maximum-finder orig-proc tolerance threshold output-type true true)))

(defn to-grayscale [image]
  (let [conv (ImageConverter. image)]
    (.convertToGray8 conv)
    image))

(defn gaussian-blur [processor]
  (let [gaussian (GaussianBlur.)]
    (.blurGaussian gaussian processor 1 1 0.01)
    processor))

(defn invert [processor]
  (.invert processor)
  processor)

(defn distance-map [processor]
  (let [edm (EDM.)]
    (.toEDM edm processor)
    processor))

(defn read-image [file]
  (->> file
       (str "examples/")
       io/resource
       javax.imageio.ImageIO/read
       (ImagePlus. file)
       to-grayscale
       processor))

(defn local-maxima
  ([vals initial]
     (let [vals (vec vals)
           size (count vals)
           rad 3
           find-max (fn [center]
                      (->> (range (- rad) (inc rad))
                           (map #(+ center %))
                           (remove neg?)
                           (remove #(>= % size))
                           (apply max-key vals)))]
       (loop [cur initial]
         (let [mx (find-max cur)]
           (if (= mx cur)
             cur
             (recur mx))))))
  ([vals]
     (let [step 5]
       (->> (range 0 (count vals) step)
            (map #(local-maxima vals %))
            distinct))))

(defn pixels [processor]
  (->> processor
       .getPixels
       seq
       (map #(bit-and % 255))))



(defn cells-candidates-image [^ImagePlus image]
  (let [dup (doto (.duplicate image)
                   (IJ/run "Invert" "")
                   (IJ/run "Distance Map" ""))]
    (find-maxima-on-image dup)))

(defn cells-candidates [^ImagePlus image]
  (let [^ByteProcessor maxima (cells-candidates-image image)]
    (for [^int y (range (.getHeight maxima))
          ^int x (range (.getWidth maxima))
          :when (= (.get maxima x y) 255)]
      [x y])))

(defn binary-nono [processor]
  (let [hist (histogram (center-third processor))
        [light dark] (->> (local-maxima hist)
                          (sort-by #(nth hist %))
                          reverse
                          (take 2))
        middle (* (+ light dark) 1/2)
        dup (duplicate processor)]
    (.threshold dup middle)
    dup))

(defn adaptive-threshold [proc]
  (let [thresholder (fiji.threshold.Auto_Local_Threshold.)
        im (image (duplicate proc))]
    (-> (.exec thresholder im "Mean" 30 20 0 true)
        seq
        first
        processor)))

;(show (adaptive-threshold (read-image "nono5.jpg")))

;(show (read-image "nono3.png"))



(def images (map #(str "nono" % ".jpg") [4 5 6 7]))

(def orig (-> "nono5.jpg" read-image))



(show orig)

#_(doseq [name images]
  (show (binary-nono (read-image name))))

#_(->> orig
     binary-nono
     distance-map
;     find-maxima-on-image
     show)

#_(-> (center-third orig)
;    gaussian-blur
    pixels
    (charts/histogram :nbins 50)
    core/view)

