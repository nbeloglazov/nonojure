(ns recognition.core
  (:import [org.opencv.core Core]
           org.opencv.imgproc.Imgproc
           org.opencv.utils.Converters)
  (:require [recognition
             [utils :as u]
             [morphology :as mor]]
            [incanter
             [core :as icore]
             [charts :as icharts]]))

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

(defn fit-to-1000! [mat]
  (let [w (.cols mat)
        h (.rows mat)]
    (u/resize! mat (/ 1000 (max w h)))))

(defn border-mask [size borders]
  (println borders)
  (letfn [(border?
            ([y x type]
               (case type
                 :left (zero? x)
                 :right (= (dec size) x)
                 :up (zero? y)
                 :down (= (dec size) y)))
            ([y x]
               (some true? (map #(border? y x %) borders))))
          (center? [y x]
            (= (quot size 2) y x))]
    (for [y (range size)]
      (for [x (range size)]
        (cond (border? y x) -1
              (center? y x)  1
              :default       0)))))

(defn remove-noise [mat & borders]
  (let [masks (->> (map #(border-mask 8 %) borders)
                   (map mor/hom-mask))]
    (loop [cur (reduce mor/thinning mat masks)
           prev (.clone mat)
           it 0]
      (println it)
      (if (or (u/zero-mat? (u/subtract prev cur prev))
              (= it 10))
        cur
        (recur (reduce mor/thinning cur masks)
               cur
               (inc it))))))

(def cross-patterns
  (letfn [(parse-and-rotate [mask]
            (->> (mor/hom-mask mask)
                 (iterate (partial map u/rotate90))
                 (take 2)))]
    (cons (mor/hom-mask [[-1  1 -1]
                         [ 1  1  1]
                         [-1  1 -1]])
          (mapcat parse-and-rotate
                  [
                   [[-1  1 -1]
                    [ 1  1 -1]
                    [-1  1  1]]

                   [[-1  1  1]
                    [ 1  1 -1]
                    [-1  1 -1]]

                   [[-1 -1  1 -1]
                    [-1 -1  1  1]
                    [ 1  1 -1 -1]
                    [-1  1 -1 -1]]

                   [[-1  1 -1 -1]
                    [-1  1  1  1]
                    [ 1  1  1 -1]
                    [-1 -1  1 -1]]

                   [[-1 -1  1 -1]
                    [ 1  1  1 -1]
                    [-1  1  1  1]
                    [-1  1 -1 -1]]
                   ]))))

(defn find-intersections [mat]
  (->> cross-patterns
       (map #(mor/hit-or-miss mat %))
       (reduce #(u/mat-or %1 %2 %2))))

(defn black-pixels [mat]
  (letfn [(column [idx]
            (let [list (java.util.ArrayList.)]
              (Converters/Mat_to_vector_uchar (.col mat idx) list)
              list))
          (black-pixels [x col]
            (map-indexed #(if (zero? %2) [x %1] nil) col))]
    (->> (.cols mat)
         range
         (map column)
         (map-indexed black-pixels)
         (apply concat)
         (remove nil?))))


#_(


   (->> "nono5.jpg"
      u/read
      fit-to-1000!
      recognition.opencv/adaptive-threshold!
;      u/show
      u/invert!
      mor/skeleton
      u/invert!
      u/show
      u/invert!
      find-intersections
      u/invert!
      (def crs)
;      u/show
;      (remove-noise [:up :left :right])
      )
   (def px (black-pixels crs))
   (count px)

   (u/show crs)
   )

#_(-> "nono5.jpg" u/read adaptive-threshold! u/show )

#_(show (read "nono5.jpg"))
