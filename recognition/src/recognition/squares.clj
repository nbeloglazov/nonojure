(ns recognition.squares
  (:require [recognition
             [utils :as u]
             [trace :refer [with-scope]]]
            [kdtree :as kd]
            [incanter
             [core :as core]
             [charts :as charts]
             [stats :as stats]]
            clojure.set)
  (:import [net.sf.javaml.core.kdtree KDTree]))

(defprotocol GenericKDTree
  (delete [this point])
  (nearest-neighbor [this point n]))

(defn build-tree [points type]
  (let [points (shuffle points)]
   (case type
     :clojure (kd/build-tree points)
     :java (let [tree (KDTree. 2)]
             (doseq [point points]
               (.insert tree (into-array Double/TYPE point) point))
             tree)
     (throw (IllegalArgumentException. (str "Not supported tree type: " type))))))

(extend-protocol GenericKDTree
  kdtree.Node
  (delete [this point] (kd/delete this point))
  (nearest-neighbor [this point n] (kd/nearest-neighbor this point n))
  KDTree
  (delete [this point]
    (->> (into-array Double/TYPE point)
         (.delete this))
    this)
  (nearest-neighbor [this point n]
    (let [key (into-array Double/TYPE point)
          neibs (seq (.nearest this key n))
          dst-sqrd (fn [p2]
                     (apply + (map (fn [v1 v2]
                                     (* (- v1 v2)
                                        (- v1 v2)))
                                   point p2)))]
      (map (fn [p] {:point p :dist-squared (dst-sqrd p)}) neibs))))

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
  (rest (nearest-neighbor tree point 5)))

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
  (letfn [(real-neib? [neib]
            (<= (* 0.8 expected-dist) (:dist-squared neib) (* 1.2 expected-dist)))
          (dir-entry [neib]
            (let [neib (:point neib)]
              [(direction point neib) (mapv int neib)]))]
    (->> (neib-4 tree point)
         (filter real-neib?)
         (map dir-entry)
         (into {}))))

(defn build-points-neighbourhood [points]
  (with-scope :build-neighbourhood
    (let [type :java
          tree (with-scope :build-kd-tree (build-tree points type))
          average-dist (with-scope :calculate-average-dist (average-dist tree points))
          neibs-map (fn [tree points]
                      (for [point points]
                        [point (neibs tree point average-dist)]))
          good-point? (fn [[point neibs]]
                        (> (count neibs) 1))]
      (with-scope :build
        (loop [points points
               tree tree]
          (let [neibs (neibs-map tree points)
                {good true bad false} (group-by good-point? neibs)]
            (if (empty? bad)
              (into {} good)
              (recur (map first good)
;                     (build-tree (map first good) type)
                     (reduce delete tree (map first bad))
                     ))))))))

(defn move [dir pos]
  (map + pos
       (case dir
         :left [-1 0]
         :right [1 0]
         :up [0 -1]
         :down [0 1])))

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
                    (doall (concat rst (vals (neibs cur))))
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
  (with-scope :normalize
    (let [min-pos (->> (keys positions)
                       (reduce #(doall (map min %1 %2))))]
      (into {} (for [[pos point] positions]
                 [(map - pos min-pos) point])))))

(defn find-largest-component [neibs]
  (with-scope :find-largest-component
    (->> (find-all-components neibs)
         (sort-by count)
         last)))

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
  (with-scope :restore-missing-points
    (loop [neibs neibs
           positions positions]
      (let [neibs (select-keys neibs (keys positions))
            missing (missing-neibs positions neibs)]
        (if (empty? missing)
          [neibs positions]
          (let [missing-pos (calc-real-positions missing)
                new-neibs (merge-neibs-with-missing neibs missing missing-pos)
                new-positions (merge positions
                                     (clojure.set/map-invert missing-pos))]
            (recur new-neibs new-positions)))))))

(defn find-boundaries [positions]
  (let [positions (set positions)
        boundary (fn [dir aggr selector]
                   (->> positions
                        (remove #(contains? positions (move dir %)))
                        (map selector)
                        u/majority))]
    {:left (boundary :left min first)
     :right (boundary :right max first)
     :up (boundary :up min second)
     :down (boundary :down max second)}))

(defn extrapolate [coord-by-pos pos]
  (let [extr (for [dir [:left :right :up :down]
                   :let [closest (coord-by-pos (move dir pos))
                         farthest (coord-by-pos (move dir (move dir pos)))]
                   :when (and closest farthest)]
               (map + closest (map - closest farthest)))]
    (if-not (empty? extr)
      (map stats/mean (apply map vector extr))
      nil)))

(defn add-borders
  ([coord-by-pos dir on-border?]
     (->> (for [pos (keys coord-by-pos)
                :when (and (on-border? pos)
                           (not (contains? coord-by-pos (move dir pos))))
                :let [new-neib (extrapolate coord-by-pos (move dir pos))]
                :when new-neib]
            [(move dir pos) new-neib])
          (into coord-by-pos)))
  ([coord-by-pos]
     (with-scope :add-border-points
       (let [{:keys [left right up down]} (find-boundaries (keys coord-by-pos))]
         (reduce #(apply add-borders %1 %2) coord-by-pos
                 [[:left #(= left (first %))]
                  [:right #(= right (first %))]
                  [:up #(= up (second %))]
                  [:down #(= down (second %))]])))))

(defn build-squares [coord-by-pos]
  (with-scope :build-squares
    (letfn [(build [pos]
              (let [square (map coord-by-pos [pos
                                              (move :right pos)
                                              (move :down (move :right pos))
                                              (move :down pos)])]
                (if (some nil? square)
                  nil
                  square)))]
      (into {} (for [pos (keys coord-by-pos)
                     :let [sq (build pos)]
                     :when sq]
                 [pos sq])))))

#_(

   (def nbs (neibs-all-and-filter c/px))

   (def pos (find-largest-component nbs))

   (let [nbs (neibs-all-and-filter c/px)
         pos (find-largest-component nbs)
         [new-nbs new-pos] (find-and-add-missing nbs pos)]
     (def nbs new-nbs)
     (def pos new-pos)
     (def sqs (->> new-pos
                   clojure.set/map-invert
                   add-borders
                   build-squares)))

   (find-boundaries (set (vals pos)))

   (def mrgd (merge-neibs-with-missing nbs mis mis-pos))

   (nbs [49 251])

   (mrgd [49 251])

   (u/show c/crs)

   (->> nbs
        (remove #(contains? (second %) :left))
        keys)

   

   (let [cl (.clone c/crs)]
     (reduce draw-quad! cl (vals sqs))
     (u/show cl))

   (add-borders (clojure.set/map-invert pos))

   (let [cl (.clone c/crs)
         new-pos (add-borders (clojure.set/map-invert pos))
         diff (clojure.set/difference (set (vals new-pos))
                                      (set (keys pos)))]
     (draw! cl :circle (keys pos))
     (draw! cl :square diff)
     (u/show cl))

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
