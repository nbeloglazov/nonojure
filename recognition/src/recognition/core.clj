(ns recognition.core
  (:import [org.opencv.core Core]
           org.opencv.imgproc.Imgproc
           org.opencv.utils.Converters)
  (:require [recognition
             [utils :as u]
             [morphology :as mor]
             [squares :as sq]]
            [incanter
             [core :as icore]
             [charts :as icharts]]
            clojure.set))

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

(defn has-digit? [mat [[x1 y1] _ [x2 y2] _]]
  (let [width (quot (- x2 x1) 4)
        height (quot (- y2 y1) 4)
        sample (.submat mat (+ y1 height) (+ y1 height height height)
                            (+ x1 width)  (+ x1 width width width))
        area (* (.cols sample) (.rows sample))
        black-pixels (- area (Core/countNonZero sample))]
    (> black-pixels 3)))

(defn largest-same-kind-seq [kind-fn positions selector good-pos?]
  (->> (filter #(good-pos? (first %)) positions)
       (sort-by #(selector (first %)))
       (partition-by #(kind-fn (second %)))
       (sort-by count)
       (last)
       (map first)
       (map selector)
       ((fn [vals] [(first vals) (last vals)]))))

(defn find-work-field [mat positions]
  (let [[width height] (->> (keys positions)
                            (reduce #(doall (map max %1 %2))))
        [x1 x2] (largest-same-kind-seq #(has-digit? mat %)
                                       positions
                                       first
                                       #(= (last %) (quot height 2)))
        [y1 y2] (largest-same-kind-seq #(has-digit? mat %)
                                       positions
                                       second
                                       #(= (first %) (quot width 2)))]
    [[x1 y1] [x2 y2]]))

(defn draw-quad! [mat [p1 p2 p3 p4]]
     (reduce #(apply u/draw-line! %1 %2) mat
             [[p1 p2]
              [p2 p3]
              [p3 p4]
              [p4 p1]]))

#_(

   (sqs [1 4])

   (->> "nono5.jpg"
      u/read
      fit-to-1000!
      adaptive-threshold!
;      u/show
      u/invert!
      mor/skeleton
      u/invert!
      u/show
      (#(do (def im (.clone %)) %))
      u/invert!
      find-intersections
      u/invert!
      (def crs)
;      u/show
;      (remove-noise [:up :left :right])
      )

   (let [cl (.clone crs)]
     (->> (vals sqs)
;          (filter #(has-digit? im %))
          (reduce draw-quad! cl)
          (u/show)))

   (u/show im)

   (def px (doall (black-pixels crs)))
   (count px)

   (let [nbs (sq/neibs-all-and-filter px)
         pos (sq/find-largest-component nbs)
         [new-nbs new-pos] (sq/find-and-add-missing nbs pos)]
     (def nbs new-nbs)
     (def pos new-pos)
     (def sqs (->> new-pos
                   clojure.set/map-invert
                   sq/add-borders
                   sq/build-squares)))

   (u/show crs)

   (find-work-field im sqs)

   )

#_(-> "nono5.jpg" u/read adaptive-threshold! u/show )

#_(show (read "nono5.jpg"))
