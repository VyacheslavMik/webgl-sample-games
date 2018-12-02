(ns games.robot-rampage.computer-terminal
  (:require [games.robot-rampage.sprite :as sprite]
            [games.robot-rampage.enemy-manager :as enemy-manager]
            [games.robot-rampage.storage :as s]
            [games.robot-rampage.utils :as u]))

(def min-spawn-time 6)

(defn new-computer-terminal [active-sprite inactive-sprite map-location]
  {:map-location map-location
   :active? true
   :last-spawn-counter 0
   :active-sprite active-sprite
   :inactive-sprite inactive-sprite})

(defn circle-colliding? [terminal other-center radius]
  (if (:active? terminal)
    (sprite/circle-colliding? (:active-sprite terminal) other-center radius)
    false))

(defn deactivate [terminal]
  (assoc terminal :active? false))

(defn try-spawn-enemy [terminal]
  (if (and (> (:last-spawn-counter terminal) min-spawn-time)
           (> (u/vector-distance
               (sprite/world-center (:active-sprite terminal))
               (sprite/world-center
                (get-in @s/context [:player :base-sprite])))
              128)
           (< (count (:enemies @s/context)) enemy-manager/max-active-enemies))
    (do
      (enemy-manager/add-enemy (:map-location terminal))
      (assoc terminal :last-spawn-counter 0))
    terminal))

(defn update* [terminal elapsed]
  (if (:active? terminal)
    (-> terminal
        (update :last-spawn-counter + elapsed)
        (try-spawn-enemy)
        (update :active-sprite sprite/update* elapsed))
    (update terminal :inactive-sprite sprite/update* elapsed)))

(defn draw* [terminal]
  (if (:active? terminal)
    (sprite/draw* (:active-sprite terminal))
    (sprite/draw* (:inactive-sprite terminal))))
     
