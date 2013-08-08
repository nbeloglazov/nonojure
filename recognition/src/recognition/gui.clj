(ns recognition.gui
  (:require [seesaw
             [core :as sc]
             [mig :as mig]
             [tree :as st]
             [icon :as si]
             [invoke :refer [invoke-later invoke-now]]
             [border :as sb]
             [chooser :refer [choose-file]]
             [graphics :as sg]
             [table :refer [table-model]]
             [font :as sf]]
            [clojure.java.io :as io]
            [recognition
             [core :as core]
             [trace :as trace]
             [utils :as utils]])
  (:import [javax.swing.tree DefaultTreeCellRenderer DefaultTreeModel DefaultMutableTreeNode]
           [java.awt.image BufferedImage]))


(def icons (into {} (for [status [:done :progress]]
                      [status (si/icon (io/resource (format "icons/%s-icon.png" (name status))))])))

(def sq-size 30)
(def font (sf/font :size 20))

(def image (atom nil))
(def image-file (atom nil))
(def recognized-image (atom nil))
(def solution (atom nil))
(def running-task (atom nil))

(declare frame)

(defn revalidate [frame]
  (doto (.. frame getContentPane)
    .revalidate
    .repaint)
  (.pack frame))

(defn open-image []
  (choose-file
   :type :open
   :multi? false
   :filters [["Images" ["jpg" "jpeg" "bmp" "png"]]]
   :remember-directory? true))

(defn tree-cell-renderer []
  (proxy [DefaultTreeCellRenderer] []
    (getTreeCellRendererComponent [tree value _ _ _ _ _]
      (let [{:keys [name status]} (.getUserObject value)]
        (.setClosedIcon this (icons status))
        (proxy-super getTreeCellRendererComponent tree (clojure.core/name name)
                     false false false 0 false)))))

(defn expand-all! [tree]
  (loop [ind 0]
    (when (not= ind (.getRowCount tree))
      (.expandRow tree ind)
      (recur (inc ind)))))

(defn tree-node [name status]
  (DefaultMutableTreeNode. {:name (keyword name)
                            :status status}))

(defn create-tree-model [picture]
  (DefaultTreeModel. (tree-node picture :progress)))

(def tree (sc/tree :model (create-tree-model "...")
                   :editable? false
                   :renderer (tree-cell-renderer)
                   :border (sb/line-border)))

(defn reset-tree [file]
  (.setModel tree (create-tree-model file)))

#_(let [model (.getModel tree)
      root (.getRoot model)]
 (.insertNodeInto model (tree-node (str "step" (rand-int 10)) :done) root (.getChildCount root))
 (expand-all! tree)
 (.getChildCount root))

(defn load-image [_]
  (when-let [file (open-image)]
    (reset! image (javax.imageio.ImageIO/read file))
    (reset! image-file (.getAbsolutePath file))
    (reset-tree (.getName file))
    (revalidate frame)))

(defn draw-image [comp gr image]
  (when-not (nil? image)
    (let [scale (double (min (/ (.getWidth comp)
                                (.getWidth image))
                             (/ (.getHeight comp)
                                (.getHeight image))))]
     (.drawImage gr image 0 0 (* scale (.getWidth image)) (* scale (.getHeight image)) nil))))

(defn trace-handler [tree]
  (let [model (sc/config tree :model)
        root (.getRoot model)
        path (atom (list root))]
    (fn [{:keys [scope type]}]
      (let [scope (.replaceAll (name scope) "-" " ")
            node (tree-node scope (if (= type :begin) :progress :done))
            top (first @path)]
       (invoke-now
        (if (= type :begin)
          (do
            (.insertNodeInto model node top (.getChildCount top))
            (swap! path conj node))
          (do
            (.setUserObject top (.getUserObject node))
            (.nodeChanged model top)
            (swap! path rest)))
        (expand-all! tree))))))

(defn to-table-model [{:keys [field-size digits-size digits]}]
  (let [[f-w f-h] field-size
        [d-w d-h] digits-size
        {:keys [left up]} digits
        to-digit (fn [digits]
                   (apply str (map first digits)))
        field (->> (repeat (+ f-w d-w) "0")
                   vec
                   (repeat (+ f-h d-h))
                   vec)
        update-row (fn [field ind row]
                     (let [diff (- d-w (count row))]
                      (reduce #(assoc-in %1 [(+ d-h ind) (+ %2 diff)] (to-digit (nth row %2)))
                              field
                              (range (count row)))))
        update-column (fn [field ind column]
                        (let [diff (- d-h (count column))]
                          (reduce #(assoc-in %1 [(+ %2 diff) (+ d-w ind)] (to-digit (nth column %2)))
                                  field
                                  (range (count column)))))
        update-part (fn [field sub-update-fn part]
                      (reduce #(sub-update-fn %1 %2 (nth part %2))
                              field (range (count part))))]
    (table-model :rows
                 (-> field
                     (update-part update-row left)
                     (update-part update-column up)))))

(defn centered-text [gr text [x y] [w h]]
  (let [w2 (/ (.. gr (getFontMetrics) (stringWidth text))
              -2)
        h2 (.. gr (getFontMetrics) (getDescent))]
    (sg/string-shape (+ x (/ w 2) w2) (+ y (/ h 2) h2 5) text)))

(defn draw-solution [{:keys [field-size digits-size digits]}]
  (let [[w h] (map #(* sq-size %) (map + field-size digits-size))
        [d-w d-h] digits-size
        im (BufferedImage. w h BufferedImage/TYPE_BYTE_BINARY)
        to-digit (fn [digits]
                   (apply str (map first digits)))
        gr (.getGraphics im)
        style (sg/style :foreground :black :font font :stroke 2)
        style-fat (sg/style :foreground :black :stroke 5)]
    (sg/draw gr (sg/rect 0 0 (dec w) (dec h)) (sg/style :background :white :foreground :black))
    (doseq [x (range 0 w sq-size)]
      (sg/draw gr (sg/line x 0 x h) style))
    (doseq [y (range 0 h sq-size)]
      (sg/draw gr (sg/line 0 y w y) style))
    (doseq [x (range (* sq-size d-w) w (* 5 sq-size))]
      (sg/draw gr (sg/line x 0 x h) style-fat))
    (doseq [y (range (* sq-size d-h) h (* 5 sq-size))]
      (sg/draw gr (sg/line 0 y w y) style-fat))
    (doseq [ind-x (range (last field-size))
            :let [column (-> digits :up (nth ind-x))]
            ind-y (range (count column))
            :let [dig (-> digits :up (nth ind-x) (nth ind-y) to-digit)
                  x (* sq-size (+ ind-x d-w))
                  y (* sq-size (+ ind-y (- d-h (count column))))]]
      (sg/draw gr (centered-text gr dig [x y] [sq-size sq-size]) style))
    (doseq [ind-y (range (first field-size))
            :let [row (-> digits :left (nth ind-y))]
            ind-x (range (count row))
            :let [dig (-> digits :left (nth ind-y) (nth ind-x) to-digit)
                  x (* sq-size (+ ind-x (- d-w (count row))))
                  y (* sq-size (+ ind-y d-h))]]
      (sg/draw gr (centered-text gr dig [x y] [sq-size sq-size]) style))
    im))


;(def tst (assoc-sizes (core/recognize (.getAbsolutePath (java.io.File. "resources/examples/nono4.jpg")))))

;(keys tst)

(defn assoc-sizes [solution]
  (let [{:keys [left up]} (:digits solution)
        [f-w f-h] (map count [up left])
        [d-w d-h] (map #(->> %
                             (map count)
                             (reduce max 0))
                       [left up])]
    (assoc solution
      :field-size [f-w f-h]
      :digits-size [d-w d-h])))

(defn update-table [solution]
  (sc/config! (sc/select frame [:#solution]) :model (to-table-model solution)))


(defn recognize []
  (when-let [file @image-file]
    (let [sol (-> (trace/with-handler (trace-handler tree)
                    (core/recognize file))
                  (assoc-sizes))
          model (sc/config tree :model)
          root (.getRoot model)]
      (reset! solution sol)
      (invoke-now
       (.setUserObject root (assoc (.getUserObject root)
                              :status :done))
       (.nodeChanged model root)
       (reset! recognized-image (draw-solution sol))
       (update-table sol)
       (revalidate frame)))))


(defn start-stop [_]
  (let [button (sc/select frame [:#start-stop])]
    (if-let [task @running-task]
      (do
        (.stop task)
        (reset! running-task nil)
        (sc/config! button :text "Start"))
      (do
        (if-let [file @image-file]
          (reset-tree file))
        (reset! running-task
                (Thread. #(do (recognize)
                              (reset! running-task nil)
                              (invoke-now (sc/config! button :text "Start")))))
        (sc/config! button :text "Abort")
        (.start @running-task)))))

(defn get-layout []
  (mig/mig-panel :constraints ["" "[grow, align center][250!, align center][grow, align center]" "[50!][grow]"]
                 :items [[(sc/button :text "Load"
                                     :listen [:action load-image])]
                         [(sc/button :text "Start"
                                     :id :start-stop
                                     :listen [:action start-stop]) "wrap"]
                         [(sc/border-panel :paint (fn [comp gr] (draw-image comp gr @image))) "grow"]
                         [tree "grow"]
                         [(sc/border-panel :paint (fn [comp gr] (draw-image comp gr @recognized-image)))
                          #_(sc/table :id :solution) "grow"]]))

(def frame (doto (sc/frame :title "Nonogram recognizer"
                           :content (get-layout)
                           :minimum-size [1200 :by 800])
             sc/pack!
             (.setLocationRelativeTo nil)
             sc/show!))
