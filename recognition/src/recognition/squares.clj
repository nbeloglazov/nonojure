(ns recognition.squares
  (:require [recognition.core :refer :all]
            [kdtree :as kd]
            [incanter
             [core :as core]
             [charts :as charts]
             [distributions :as dist]]))

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

(defn center [blob]
  (let [p (.getCenterOfGravity blob)]
    (with-meta [(.y p) (.x p)]
      {:blob blob})))

(defn direction [[f-y f-x] [t-y t-x]]
  (let [angle (core/atan2 (- t-y f-y) (- t-x f-x))
        pi4 (/ Math/PI 4)]
    (cond (<= (- pi4) angle pi4) :right
          (<= pi4 angle (* 3 pi4)) :down
          (<= (* pi4 -3) angle (- pi4)) :up
          (or (<= (* 3 pi4) angle (+ Math/PI 0.001))
              (<= (- (- Math/PI) -0.001) angle (* pi4 -3))) :left
          :else (throw (IllegalArgumentException. (str angle))))))

(defn neibs [tree blob]
  (let [p (center blob)
        dist (.getEnclosedArea blob)
        real-neib? (fn [{got-dist :dist-squared}]
                     (<= (* 0.8 dist) got-dist (* 2 dist)))
        dir-entry (fn [{:keys [dist-squared point] :as res}]
                    [(direction p point)
                     (:blob (meta res))])]
    (->> (kd/nearest-neighbor tree p 5)
         (rest)
         (filter real-neib?)
         (map dir-entry)
         (reverse)
         (into {}))))

(defn tree [blobs]
  (kd/build-tree (map center blobs)))

(defn neibs-all [blobs]
  (let [tr (tree blobs)]
    (into {} (for [blob blobs]
               [blob (neibs tr blob)]))))

(defn move [dir pos]
  (map + pos
       (case dir
         :left [0 -1]
         :right [0 1]
         :up [-1 0]
         :down [1 0])))

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

(draw-blobs orig
            (->> bls
                 (remove has-digit?)
                 neibs-all
                 find-largest-component
                 keys))

(def tr (tree bls))

(def nbs (neibs-all bls))

;(find-component nbs)

(let [[bl neibs] (nth (seq nbs) 4)]
  (draw-blobs orig (cons bl (vals neibs))))

;(draw-blobs orig (take 1 bls))
;(draw-blobs orig bls)

(kd/nearest-neighbor (tree bls) [0 0] 4)

#_(-> (kd/build-tree points)
    (kd/nearest-neighbor [0 0] 10000)
    (->> (map meta)))

#_(-> (for [i (range 5)]
      (with-meta [i i] {:value i}))
    (kd/build-tree)
    (kd/delete [0 0])
    (kd/nearest-neighbor [0 0] 4)
    (->> (map meta)))
