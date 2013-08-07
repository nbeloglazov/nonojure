(ns recognition.gui
  (:require [seesaw
             [core :as sc]
             [mig :as mig]
             [tree :as st]
             [icon :as si]]
            [clojure.java.io :as io])
  (:import [javax.swing.tree DefaultTreeCellRenderer DefaultTreeModel DefaultMutableTreeNode]))


(def icons (into {} (for [status [:done :progress]]
                      [status (si/icon (io/resource (format "icons/%s-icon.png" (name status))))])))

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

(def tree (sc/tree :model (create-tree-model "hsdf")
                   :editable? false
                   :renderer (tree-cell-renderer)))


(let [model (.getModel tree)
      root (.getRoot model)]
 (.insertNodeInto model (tree-node (str "step" (rand-int 10)) :done) root (.getChildCount root))
 (expand-all! tree)
 (.getChildCount root))

(defn get-layout []
  (mig/mig-panel :constraints ["" "[grow, align center][100!, align center][grow, align center]" "[50!][grow]"]
                 :items [[(sc/button :text "Load")]
                         [(sc/button :text "Start") "wrap"]
                         [(sc/button :text "hi")]
                         [tree]
                         [(sc/button :text "hi")]]))




(-> (sc/frame :title "hello"
              :content (get-layout))
    sc/pack!
    sc/show!)
