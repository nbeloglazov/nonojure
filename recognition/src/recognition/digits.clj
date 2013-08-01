(ns recognition.digits
  (:require [recognition
             [core :as c]
             [utils :as u]
             [morphology :as mor]]
            [enclog
             [nnets :as nnets]
             [training :as train]
             [util :as en-ut]]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import [org.opencv.core Core Mat CvType Scalar MatOfFloat Size]
           org.opencv.imgproc.Imgproc))

(def size 30)

(def center (quot size 2))

(defn clear-borders! [digit]
  (doseq [m (concat (map #(.row digit %) [0 1 (- size 2) (- size 1)])
                    (map #(.col digit %) [0 1 (- size 2) (- size 1)]))]
    (.setTo m (Scalar. 255.0)))
  digit)

(defn rect-center [rect]
  [(+ (.x rect) (/ (.width rect) 2))
   (+ (.y rect) (/ (.height rect) 2))])

(defn find-bad-contours [conts]
  (letfn [(too-small? [rect]
            (< (.height rect) (/ size 2)))
          (too-close-to-borders? [rect]
            (let [[x y] (rect-center rect)]
              (and (or (< x (* size 1/7))
                       (> x (* size 6/7)))
                   (< (.width rect) 4))))]
    (filter #(let [rect (Imgproc/boundingRect %)]
               (or (too-small? rect)
                   (too-close-to-borders? rect)))
            conts)))

(defn remove-contours! [mat conts]
  (Imgproc/drawContours mat conts -1 (Scalar. 255.0) -1)
  mat)

(defn top-contours [mat]
  (let [l (java.util.ArrayList.)
        m  (Mat.)
        cl (u/invert! (u/clone mat))]
    (Imgproc/findContours cl l m Imgproc/RETR_EXTERNAL Imgproc/CHAIN_APPROX_SIMPLE)
    l))

(defn center-digit [dig]
  (let [[fst & rst] (top-contours dig)]
    (if (and (not (nil? fst)) (nil? rst))
      (let [[x y] (rect-center (Imgproc/boundingRect fst))
            transf-mat (-> (into-array Float/TYPE [1 0 (- center x) 0 1 (- center y)])
                           (MatOfFloat.)
                           (.reshape 1 2))
            res (Mat.)]
        (Imgproc/warpAffine dig res transf-mat (Size. size size) Imgproc/INTER_NEAREST Imgproc/BORDER_CONSTANT (Scalar. 255.0))
        res)
      dig)))

(defn clear-noise! [digit]
  (let [digit (clear-borders! digit)
        bad (find-bad-contours (top-contours digit))]
    (remove-contours! digit bad)))

(defn find-separator [digit]
  (let [weight (fn [n]
                 (-> (.col digit n) Core/sumElems .val (aget 0)))]
    (apply max-key (memoize weight) (range 5 (inc center)))))

(defn separate [digit]
  (let [subdigit (fn [from to]
                   (let [res (Mat. size size CvType/CV_8UC1 (Scalar. 255.0))
                         width (- to from)
                         dst-from (- center (quot width 2))
                         dst (.submat res 0 (dec size) dst-from (+ dst-from width))
                         src (.submat digit 0 (dec size) from to)]
                     (.copyTo src dst)
                     res))
        conts (sort-by #(first (rect-center (Imgproc/boundingRect %))) (top-contours digit))]
    (if (= (count conts) 1)
     (let [col (find-separator digit)]
       (map clear-noise! [(subdigit 0 (dec col))
                          (subdigit (inc col) (dec size))]))
     (->> (map #(Imgproc/boundingRect %) conts)
          identity
          (map #(subdigit (.x %) (+ (.x %) (.width %))))
          (map clear-noise!)))))

;(def d6 (clear-noise! (u/quad-to-rect c/orig (-> c/strut :left (nth 9) (nth 0)) [size size])))

(def net (en-ut/eg-load "network.eg"))

(defn mat-to-array [mat]
  (let [m (Mat.)]
   (.convertTo mat m CvType/CV_32F (/ 1.0 255))
   (-> (.reshape m 1 1)
      (MatOfFloat.)
      (.toList))))

(defn recognize-digit [dig]
  (let [score (fn [output]
                (let [mx (apply max output)]
                  (- (+ mx mx) (apply + output))))
        recognize (fn [dig]
                    (->> (mat-to-array dig)
                    (train/data :basic)
                    (.compute net)
                    .getData
                    seq))
        to-digit (fn [output]
                   (apply max-key second (map-indexed vector output)))
        single (recognize dig)
        [dbl-l dbl-r] (->> (separate dig)
                           (map center-digit)
                           (map u/blur!)
                           (map recognize))]
    (println dbl-l dbl-r)
    (if (> (score single)
           (* (score dbl-l)
              (score dbl-r)))
      [(to-digit single)]
      (map to-digit [dbl-l dbl-r]))))

(defn recognize-all-digits [mat nono]
  (letfn [(extract [positions]
            (u/blur! (center-digit (clear-noise! (u/quad-to-rect mat positions [size size])))))
          (recognize-row [row]
            (map #(recognize-digit (extract %)) row))
          (recognize-part [part]
            (map recognize-row part))]
    (reduce #(update-in %1 [%2] recognize-part) nono [:left :up])))


;;; Creating train set

(defn extract-digit-images [name]
  (let [orig (->> (str name ".jpg") u/read c/fit-to-1000! c/adaptive-threshold!)
        im (->> (.clone orig) u/invert! mor/skeleton c/remove-noise u/invert!)
        real-nono (->> (str "parsed/" name ".clj") io/resource slurp edn/read-string)
        nono (c/parse-structure im)
        dig-filename (fn [value part ind1 ind2 other]
                       (format "test-set/%d.%s_%s_%d_%d%s.png" value name (clojure.core/name part) ind1 ind2 other))
        prepare (comp u/blur! center-digit)
        process-part (fn [part]
                       (let [n-part (nono part)
                             rn-part (real-nono part)]
                         (doseq [ind1 (range (count n-part))
                                 :when (= (count (n-part ind1))
                                          (count (rn-part ind1)))
                                 ind2 (range (count (n-part ind1)))]
                           (let [value (get-in rn-part [ind1 ind2])
                                 digit (-> (u/quad-to-rect orig (get-in n-part [ind1 ind2]) [size size])
                                           clear-noise!)]
                             (if (< value 10)
                               (u/save (prepare digit) (dig-filename value part ind1 ind2 ""))
                               (let [[left right] (map prepare (separate digit))]
                                 (when (and (not= (* size size) (Core/countNonZero left))
                                            (not= (* size size) (Core/countNonZero right)))
                                  (u/save left (dig-filename (quot value 10) part ind1 ind2 "_1of2"))
                                  (u/save right (dig-filename (mod value 10) part ind1 ind2 "_2of2")))))))))]
    (process-part :left)
    (process-part :up)))

#_(u/show (clear-noise! (u/quad-to-rect c/orig (-> c/strut :left (nth 4) (nth 0)) [size size])))

#_(u/show c/orig)

;;; Training neural network

(defn input [dir name]
  (mat-to-array (u/read (str dir "/" name) "")))

(defn output [name]
  (assoc [0 0 0 0 0 0 0 0 0 0] (- (int (first name)) (int \0)) 1))

(defn read-dataset [dir]
  (->> (io/file dir)
       (file-seq)
       (filter #(.isFile %))
       (map #(.getName %))
       (map #(vector (input dir %) (output %)))
       (apply map vector)
       (apply train/data :basic-dataset)))



#_(

   (def net
     (nnets/network (nnets/neural-pattern :feed-forward)
                    :activation :sigmoid
                    :input   900
                    :output  10
                    :hidden [700]))

   (def dataset (read-dataset "train-set"))

   (def test (read-dataset "test-set"))

   (def trainer (train/trainer :resilient-prop :network net :training-set dataset))

   (train/train trainer 0.001 1000 [])

   (en-ut/eg-persist net "network.eg")



   (en-ut/eg-load "")

   (extract-digit-images "nono4")

   (doseq [ind (range 4 10)]
     (println ind)
     (extract-digit-images (str "nono" ind)))

   (->> (io/file "test-set")
        (file-seq)
        (map #(.getName %))
        (map first)
        frequencies)

   (defn move-to-train [files]
     (doseq [file (take 40 (shuffle files))]
       (.renameTo file (io/file (str "train-set/" (.getName file))))))

   (->> (io/file "test-set")
        (file-seq)
        (group-by #(first (.getName %)))
        (filter (fn [[k v]]
                  (<= (int \0) (int k) (int \9))))
        (map second)
        (map move-to-train)
        (dorun))


   (let [quad (-> strut :left (nth 0) (nth 1))
         dig (clear-borders! (u/quad-to-rect orig quad [size size]))
         [left right] (separate dig (find-separator dig))]
     (def dig dig)
     (def left left)
     (def right right))

   (u/show dig)

   (u/show right)


   (def res (recognize-all-digits c/orig c/strut))

   (doseq [part [:left :up]]
     (println)
     (println part)
     (doseq [row (res part)]
       (println)
       (doseq [v row]
         (print " " (apply str (map first v))))))

   (defn digit [type row ind]
     (-> (u/quad-to-rect c/orig (-> c/strut type (nth row) (nth ind)) [size size])
         clear-noise!
         center-digit
         u/blur!))

   (def d6 (digit :left 0 0))

   (def d14 (clear-borders! (u/quad-to-rect c/orig (-> c/strut :left (nth 5) first) [size size])))

   (def d11 (clear-borders! (u/quad-to-rect c/orig (-> c/strut :left (nth 6) first) [size size])))


   (recognize-digit (u/show (digit :up 0 0)))



   (u/show d11)

   (recognize-digit (first (separate d11 (find-separator d11))))

   (recognize-digit d11)



   )
