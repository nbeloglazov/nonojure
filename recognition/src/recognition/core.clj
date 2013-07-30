(ns recognition.core
  (:import [org.opencv.core Core Mat Scalar]
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

(defn remove-noise [mat]
  (let [masks (->> [3 7 11]
                   (map #(border-mask % [:left :right :up :down]))
                   (map mor/hom-mask))]
    (reduce mor/thinning mat masks)))

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

(defn add-missing-points [[[lu-x lu-y] [rd-x rd-y]] positions]
     (let [exist (set (keys positions))
           up-digits (for [x (range lu-x (inc rd-x))
                           y (range 0 (inc lu-y))
                           :when (not (exist [x y]))]
                       [x y])
           left-digits (for [x (range 0 (inc rd-x))
                             y (range lu-y (inc rd-y))
                             :when (not (exist [x y]))]
                         [x y])
           missing (concat up-digits left-digits)
           num-of-neibs (fn [pos positions]
                          (->> [:left :right :up :down]
                               (map #(sq/move % pos))
                               (map positions)
                               (remove nil?)
                               count))]
       (loop [positions positions
              missing missing]
         (if (empty? missing)
           positions
           (let [[fst & rst] (sort-by #(- (num-of-neibs % positions)) missing)]
             (recur (assoc positions fst (sq/extrapolate positions fst))
                    rst))))))

(defn get-squares-with-digits [mat squares [[lu-x lu-y] [rd-x rd-y]]]
     (letfn [(digit-seq [start dir]
               (->> (iterate #(sq/move dir %) start)
                    (map squares)
                    (take-while #(not (nil? %)))
                    (take-while #(has-digit? mat %))
                    reverse
                    vec))]
       {:up (vec (for [x (range lu-x (inc rd-x))]
                   (digit-seq [x (dec lu-y)] :up)))
        :left (vec (for [y (range lu-y (inc rd-y))]
                     (digit-seq [(dec lu-x) y] :left)))}))

(defn draw-quad! [mat [p1 p2 p3 p4]]
     (reduce #(apply u/draw-line! %1 %2) mat
             [[p1 p2]
              [p2 p3]
              [p3 p4]
              [p4 p1]]))

(defn draw-squares [mat sqs]
  (doseq [sq sqs]
    (draw-quad! mat sq)))


(defn draw! [mat type points]
     (let [draw-fn (case type
                     :circle #(u/draw-circle! %1 %2 10)
                     :square #(u/draw-square! %1 %2 10))]
       (reduce draw-fn mat points)))

(defn white [mat]
  (u/invert! (Mat/zeros (.rows mat) (.cols mat) (.type mat))))


(defn parse-structure [mat]
  (let [intersections (->> (.clone mat)
                           u/invert!
                           find-intersections
                           u/invert!
                           black-pixels)
        nbs (sq/neibs-all-and-filter intersections)
        pos (sq/find-largest-component nbs)
        _ (let [cl (white mat)]
            (draw! cl :circle (keys pos))
            ;(u/show cl)
            )
        [nbs pos] (sq/find-and-add-missing nbs pos)
        _ (let [cl (white mat)]
            (draw! cl :circle (keys pos))
;            (u/show cl)
            )
        pos (->> (clojure.set/map-invert pos)
                 sq/add-borders
                 sq/normalize-component)
        squares (sq/build-squares pos)
        _ (def sqs squares)
        field (find-work-field mat squares)
        pos (add-missing-points field pos)
        squares (sq/build-squares pos)
        _ (let [cl (white mat)]
            (draw-squares cl (vals squares))
;            (u/show cl)
            )
        nono (get-squares-with-digits mat squares field)
        size (map - (second field) (first field) [-1 -1])]
    (assoc nono :size size)))

(defn valid-nono? [nono]
  (letfn [(sum [part]
            (apply + (flatten (part nono))))]
    (- (sum :left) (sum :up))))

#_(

   (extract-digit-images "nono5")
   (require '[clojure.edn :as edn])
   (require '[clojure.java.io :as io])


   (->> "parsed/nono6.clj"
        io/resource
        slurp
        edn/read-string
        valid-nono?
        )


   (defn ddef [im]
     (def orig (.clone im))
     im)
   (->> "nono4.jpg"
      u/read
      fit-to-1000!
      adaptive-threshold!
      ddef
      u/show
      u/invert!
      mor/skeleton
      remove-noise
      u/invert!
      u/show
      (def im))

   (let [l (java.util.ArrayList.)
         m (Mat.)
         cloned (u/invert! (u/clone orig))
         _ (Imgproc/findContours cloned l m Imgproc/RETR_LIST Imgproc/CHAIN_APPROX_SIMPLE)
         l (filter #(< 10 (Imgproc/contourArea %) 100) l)
         whi (white cloned)]
     (Imgproc/drawContours whi l -1 (Scalar. 0.0))
     (u/show whi)
;     (icore/view (icharts/histogram (map #(Imgproc/contourArea %) l) :nbins 100))
     (count l))

   (u/show orig)

   (time (def strut (parse-structure im)))

   (def ex (-> strut :left (nth 2) (nth 2)))

   (u/show (u/quad-to-rect orig ex [30 30]))

   (:size strut)

   (u/show im)

   (let [cl (white im)]
     (doseq [sqs (mapcat strut [:left :up])]
       (draw-squares cl sqs))
     (u/show cl))

   (let [cl (.clone crs)]
     (->> (vals sqs)
;          (filter #(has-digit? im %))
          (reduce draw-quad! cl)
          (u/show)))

   (u/show im)

   (def px (doall (black-pixels crs)))

   (let [
         ]
     (def nbs nbs)
     (def pos pos)
     (def sqs (->> pos
                   clojure.set/map-invert
                   sq/add-borders
                   sq/build-squares)))

   (let []
     (map count (:left nono))
;     (get-squares-with-digits im sqs field)
#_     (let [cl (.clone crs)]
       (->> (vals sqs)
            (filter #(has-digit? im %))
            (reduce draw-quad! cl)
            (u/show))))

   (u/show crs)

   (find-work-field im sqs)

   )

#_(-> "nono5.jpg" u/read adaptive-threshold! u/show )

#_(show (read "nono5.jpg"))
