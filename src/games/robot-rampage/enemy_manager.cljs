(ns games.robot-rampage.enemy-manager
  (:require [games.robot-rampage.storage :as s]
            [games.robot-rampage.enemy :as enemy]
            [games.robot-rampage.tile-map :as tile-map]))

(def initial-frame {:x 0 :y 160 :w 32 :h 32})

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

(defn update* [elapsed]
  (when (= (count (:enemies @s/context)) 0)
    (add-enemy {:x 10 :y 10}))
  (let [enemies (->> (:enemies @s/context)
                     (mapv (fn [enemy]
                             (enemy/update* enemy elapsed)))
                     (remove :destroyed?)
                     (vec))]
    (swap! s/context assoc :enemies enemies)))

(defn draw* []
  (doseq [enemy (:enemies @s/context)]
    (enemy/draw* enemy)))
