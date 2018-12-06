(ns games.gemstone-hunter.camera
  (:require [games.gemstone-hunter.utils :as u]))

(def context (atom {:position {:x 0 :y 0}
                    :view-port-width 0
                    :view-port-height 0
                    :world-rectangle {:x 0 :y 0 :width 0 :height 0}}))

(defn transform-point [point]
  (u/vector-sub point (:position @context)))

(defn transform-rectangle [rectangle]
  (-> rectangle
      (update :x - (:x (:position @context)))
      (update :y - (:y (:position @context)))))

(defn view-port-width  [] (:view-port-width @context))
(defn view-port-height [] (:view-port-height @context))

(defn view-port []
  (let [{:keys [position view-port-width view-port-height]} @context]
    (assoc position
           :width view-port-width
           :height view-port-height)))

(defn move [offset]
  (let [{:keys [position world-rectangle view-port-width view-port-height]} @context
        position {:x (u/clamp (+ (:x position) (:x offset))
                              (:x world-rectangle)
                              (- (:width world-rectangle) view-port-width))
                  :y (u/clamp (+ (:y position) (:y offset))
                              (:y world-rectangle)
                              (- (:height world-rectangle) view-port-height))}]
    (swap! context assoc :position position)))

(defn object-visible? [bounds]
  (u/rectangle-intersects? (view-port) bounds))

(defn world-to-screen-v [world-location]
  (u/vector-sub world-location (:position @context)))

(defn world-to-screen-r [world-rectangle]
  (let [position (:position @context)]
    (-> world-rectangle
        (update :x - (:x position))
        (update :y - (:y position)))))

(defn screen-to-world-v [screen-location]
  (u/vector-add screen-location (:position @context)))

(defn screen-to-world-r [screen-rectangle]
  (let [position (:position @context)]
    (-> screen-rectangle
        (update :x + (:x position))
        (update :y + (:y position)))))
