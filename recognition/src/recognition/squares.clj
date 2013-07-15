(ns recognition.squares
  (:require [recognition
             [core :as c]
             [utils :as u]]
            [kdtree :as kd]
            [incanter
             [core :as core]
             [charts :as charts]
             [stats :as stats]]
            clojure.set))

(defn has-digit? [blob]
  (let [sq-size (core/sqrt (.getEnclosedArea blob))]
   (letfn [(contour-height [pol]
             (let [rect (.getBounds pol)]
               (- (.getMaxY rect) (.getMinY rect))))
           (digit? [pol]
             (> (contour-height pol) (* 0.5 sq-size)))
           (has-cavity? [blob]
             (> (.getPerimeter blob) (* 1.2 (.getPerimeterConvexHull blob))))]
     (or (->> (.getInnerContours blob)
              (map digit?)
              (some true?))
         (has-cavity? blob)))))

(defn direction [[f-x f-y] [t-x t-y]]
  (let [angle (core/atan2 (- t-y f-y) (- t-x f-x))
        pi4 (/ Math/PI 4)]
    (cond (<= (- pi4) angle pi4) :right
          (<= pi4 angle (* 3 pi4)) :down
          (<= (* pi4 -3) angle (- pi4)) :up
          (or (<= (* 3 pi4) angle (+ Math/PI 0.001))
              (<= (- (- Math/PI) -0.001) angle (* pi4 -3))) :left
          :else (throw (IllegalArgumentException. (str angle))))))

(defn neib-4 [tree point]
  (rest (kd/nearest-neighbor tree point 5)))

(defn average-dist [tree points]
  (let [threshold (* 4 (count points) 0.1)]
    (->> points
         (mapcat #(neib-4 tree %))
         (map :dist-squared)
         sort
         (drop threshold)
         (drop-last threshold)
         (stats/mean))))

(defn neibs [tree point expected-dist]
  (let [real-neib? (fn [{got-dist :dist-squared}]
                     (<= (* 0.8 expected-dist) got-dist (* 1.2 expected-dist)))
        dir-entry (fn [{neib :point :as res}]
                    [(direction point neib)
                     (mapv int neib)])]
    (->> (kd/nearest-neighbor tree point 5)
         (rest)
         (filter real-neib?)
         (map dir-entry)
         (into {}))))


(defn neibs-all [points]
  (let [tree (kd/build-tree points)
        average-dist (average-dist tree points)]
    (into {} (for [point points
                   :let [nbs (neibs tree point average-dist)]
                   :when (> (count nbs) 1)]
               [point nbs]))))

(defn neibs-all-and-filter [points]
  (let [nbs (neibs-all points)]
    (if (= (count nbs) (count points))
      nbs
      (recur (keys nbs)))))

(defn move [dir pos]
  (map + pos
       (case dir
         :left [0 -1]
         :right [0 1]
         :up [-1 0]
         :down [1 0])))

(def op-dir {:left :right
             :right :left
             :up :down
             :down :up})

(defn find-component
  ([neibs base]
     (loop [visited #{}
            queue [base]
            positions {base [0 0]}]
       (if (empty? queue)
         positions
         (let [[cur & rst] queue
               cur-pos (positions cur)]
           (if (visited cur)
             (recur visited rst positions)
             (recur (conj visited cur)
                    (concat rst (vals (neibs cur)))
                    (reduce (fn [positions [dir blob]]
                              (assoc positions blob (move dir cur-pos)))
                            positions
                            (neibs cur))))))))
  ([neibs]
     (find-component neibs (first (keys neibs)))))

(defn find-all-components [neibs]
  (if (empty? neibs)
    []
    (let [comp (find-component neibs)
          left (apply dissoc neibs (keys comp))]
      (conj (find-all-components left) comp))))

(defn normalize-component [positions]
  (let [min-pos (->> (vals positions)
                     (reduce #(map min %1 %2)))]
    (into {} (for [[blob pos] positions]
               [blob (map - pos min-pos)]))))

(defn find-largest-component [neibs]
  (->> (find-all-components neibs)
       (sort-by count)
       last
       normalize-component))

(defn missing-neibs [positions neibs]
  (letfn [(neib-pos [point dir]
            (let [pos (move dir (positions point))]
              [pos {(op-dir dir) point}]))
          (missing [[point neibs]]
            (->> (keys neibs)
                 (reduce disj #{:left :right :up :down})
                 (map #(neib-pos point %))
                 (into {})))]
    (->> (map missing neibs)
         (apply merge-with merge)
         (filter (fn [[k v]] (> (count v) 2)))
         (into {}))))

(defn calc-real-positions [missing]
  (letfn [(calc-x [{:keys [left right up down]}]
            (stats/mean (map first (if (and left right)
                                     [left right]
                                     [up down]))))
          (calc-y [{:keys [left right up down]}]
            (stats/mean (map second (if (and up down)
                                      [up down]
                                      [left right]))))
          (position [[pos neibs]]
            [pos [(calc-x neibs) (calc-y neibs)]])]
    (into {} (map position missing))))

(defn merge-neibs-with-missing [neibs missing missing-pos]
  (let [ms (into {} (for [k (keys missing)]
                      [(missing-pos k) (missing k)]))
        add-new (fn [neibs [new-point new-neibs]]
                  (reduce (fn [neibs [dir neib]]
                            (assoc-in neibs [neib (op-dir dir)] new-point))
                          neibs
                          new-neibs))]
    (merge ms (reduce add-new neibs ms))))

(defn find-and-add-missing [neibs positions]
  (let [neibs (select-keys neibs (keys positions))
        missing (missing-neibs positions neibs)
        missing-pos (calc-real-positions missing)
        new-neibs (merge-neibs-with-missing neibs missing missing-pos)
        new-positions (merge positions
                             (clojure.set/map-invert missing-pos))]
    [new-neibs new-positions]))

#_(

   (def nbs (neibs-all-and-filter c/px))

   (def pos (find-largest-component nbs))


   (nbs [75 874])
   (nbs [90 872])

   (nbs [49 251])

   (pos [75 874])
   (pos [90 872])

   (def nbs-fil (select-keys nbs (keys pos)))

   (def mis (missing-neibs pos nbs-fil))

   (def mis-pos (calc-real-positions mis))

   (def mrgd (merge-neibs-with-missing nbs mis mis-pos))

   (nbs [49 251])

   (mrgd [49 251])

   (u/show c/crs)

   (let [nbs (select-keys nbs (keys pos))
         [new-nbs new-pos] (find-and-add-missing nbs pos)
         cl (.clone c/crs)]
     (reduce #(u/draw-circle! %1 %2 10) cl (keys nbs))
     (reduce #(u/draw-square! %1 %2 10) cl (clojure.set/difference (set (keys new-nbs)) (set (keys nbs))))
     (u/show cl)
     )

   (let [cl (.clone c/crs)]
     #_(doseq [[[x y] [c-x c-y]] cmp]
         (u/put-text! cl (str c-x " " c-y) [(- x 10) y]))
     (->> (vals mis-pos)
          (reduce #(u/draw-square! %1 %2 10) cl)
          u/show))

   (take 10 cmp)
   )

;(find-component nbs)

;(draw-blobs orig (take 1 bls))
;(draw-blobs orig bls)

#_(-> (kd/build-tree points)
    (kd/nearest-neighbor [0 0] 10000)
    (->> (map meta)))

#_(-> (for [i (range 5)]
      (with-meta [i i] {:value i}))
    (kd/build-tree)
    (kd/delete [0 0])
    (kd/nearest-neighbor [0 0] 4)
    (->> (map meta)))
