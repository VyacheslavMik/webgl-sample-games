(ns games.robot-rampage.camera
  (:require [games.robot-rampage.storage :as s]
            [games.robot-rampage.utils :as u]))

(defn init []
  (swap! s/context assoc :camera
         {:position {:x 0 :y 0}
          :view-port-width 800
          :view-port-height 600
          :world-rectangle {:x 0 :y 0 :width 1600 :height 1600}}))

(defonce container (let [container (js/PIXI.Container.)]
                     (set! (.. container -width) 800)
                     (set! (.. container -height) 600)
                     container))

(defn position []
  (get-in @s/context [:camera :position]))

(defn transform-point [point]
  (u/vector-sub point (position)))

(defn transform-rectangle [rectangle]
  (-> rectangle
      (update :x - (:x (position)))
      (update :y - (:y (position)))))

(defn view-port-width  [] (get-in @s/context [:camera :view-port-width]))
(defn view-port-height [] (get-in @s/context [:camera :view-port-height]))

(defn view-port []
  (let [cam (:camera @s/context)]
    (assoc (:position cam)
           :width (:view-port-width cam)
           :height (:view-port-height cam))))

(defn object-visible? [bounds]
  (u/rectangle-intersects? (view-port) bounds))

(defn world-rectangle []
  (get-in @s/context [:camera :world-rectangle]))

(defn move [offset]
  (let [{:keys [position world-rectangle view-port-width view-port-height]} (:camera @s/context)
        position {:x (u/clamp (+ (:x position) (:x offset))
                              (:x world-rectangle)
                              (- (:width world-rectangle) view-port-width))
                  :y (u/clamp (+ (:y position) (:y offset))
                              (:y world-rectangle)
                              (- (:height world-rectangle) view-port-height))}]
    (set! (.. container -pivot -x) (:x position))
    (set! (.. container -pivot -y) (:y position))
    (swap! s/context assoc-in [:camera :position] position)))

(defn add [sprite]
  (.. container (addChild sprite)))
