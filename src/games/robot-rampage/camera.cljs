(ns games.robot-rampage.camera
  (:require [games.robot-rampage.storage :as s]
            [games.robot-rampage.utils :as u]))

(defn init []
  (swap! s/context assoc :camera
         {:position {:x 0 :y 0}
          :view-port-size {:x 0 :y 0}
          :view-port-width 800
          :view-port-height 600
          :world-rectangle {:x 0 :y 0 :width 1600 :height 1600}}))

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
