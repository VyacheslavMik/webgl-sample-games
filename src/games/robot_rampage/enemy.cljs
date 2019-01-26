(ns games.robot-rampage.enemy
  (:require [games.robot-rampage.utils :as u]
            [games.robot-rampage.storage :as s]
            [games.robot-rampage.tile-map :as tile-map]
            [games.robot-rampage.path-finder :as path-finder]
            [games.robot-rampage.sprite :as sprite]))

(defn new-enemy [world-location initial-frame]
  (let [enemy-base (-> (sprite/new-sprite world-location initial-frame {:x 0 :y 0})
                       (assoc :collision-radius 14))
        turret-frame (update initial-frame :y + (:h initial-frame))
        enemy-claws (sprite/new-sprite world-location turret-frame {:x 0 :y 0})]
    {:enemy-base enemy-base
     :enemy-claws enemy-claws
     :enemy-speed 60
     :current-target-square (sprite/world-center enemy-base)
     :destroyed? false}))

(defn reached-target-square [enemy]
  (<= (u/vector-distance (sprite/world-center (:enemy-base enemy))
                         (tile-map/get-square-center (:current-target-square enemy)))
      2))

(defn get-new-target-square [enemy]
  (let [player-base-sprite (get-in @s/context [:player :base-sprite])]
    (path-finder/find-path (tile-map/get-square-at-pixel (sprite/world-center (:enemy-base enemy)))
                           (tile-map/get-square-at-pixel (sprite/world-center player-base-sprite)))))

(defn determine-move-direction [enemy]
  (let [reached? (reached-target-square enemy)
        current-target-square (if reached?
                                (get-new-target-square enemy)
                                (:current-target-square enemy))
        square-center (tile-map/get-square-center current-target-square)]
    {:current-target-square current-target-square
     :direction (u/vector-sub square-center (sprite/world-center (:enemy-base enemy)))}))

(defn update* [enemy elapsed]
  (if (:destroyed? enemy)
    enemy
    (let [{:keys [current-target-square direction]} (determine-move-direction enemy)
          direction (u/vector-normalize direction)
          enemy-base (-> (:enemy-base enemy)
                         (assoc :velocity (u/vector-mul direction (:enemy-speed enemy)))
                         (sprite/rotate-to direction)
                         (sprite/update* elapsed))

          direction-to-player (-> (u/vector-sub
                                   (sprite/world-center (get-in @s/context [:player :base-sprite]))
                                   (sprite/world-center enemy-base))
                                  (u/vector-normalize))
          enemy-claws (-> (:enemy-claws enemy)
                          (assoc :world-location (:world-location enemy-base))
                          (sprite/rotate-to direction-to-player))]
      (sprite/update-pixi-sprite enemy-claws)
      (assoc enemy
             :enemy-base enemy-base
             :current-target-square current-target-square
             :enemy-claws enemy-claws))))
