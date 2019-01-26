(ns games.robot-rampage.enemy-manager
  (:require [games.robot-rampage.storage :as s]
            [games.robot-rampage.enemy :as enemy]
            [games.robot-rampage.tile-map :as tile-map]))

(def initial-frame {:x 0 :y 160 :w 32 :h 32})
(def max-active-enemies 30)

(defn init []
  (swap! s/context assoc [:enemies] []))

(defn add-enemy [square-location]
  (let [start-x (:x square-location)
        start-y (:y square-location)
        square-rect (tile-map/square-world-rectangle start-x start-y)
        new-enemy (-> (enemy/new-enemy {:x (:x square-rect) :y (:y square-rect)}
                                       initial-frame)
                      (assoc :current-target-square square-location))]
    (swap! s/context update :enemies conj new-enemy)))

(defn update-enemies [enemies elapsed]
  (let [c (count enemies)]
    (loop [i 0
           acc (transient [])]
      (if (< i c)
        (let [enemy (enemy/update* (nth enemies i) elapsed)]
          (when (:destroyed? enemy)
            (.. (-> enemy :enemy-base :sprite)  destroy)
            (.. (-> enemy :enemy-claws :sprite) destroy))
          (recur (inc i) (if (:destroyed? enemy) acc (conj! acc enemy))))
        (persistent! acc)))))

(defn update* [elapsed]
  (let [enemies (update-enemies (:enemies @s/context) elapsed)]
    (swap! s/context assoc :enemies enemies)))
