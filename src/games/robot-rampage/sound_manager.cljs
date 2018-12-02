(ns games.robot-rampage.sound-manager
  (:require [games.engine :as engine]
            [games.robot-rampage.storage :as s]))

(defn play-explosion []
  (let [sounds (:explosion-sounds @s/context)
        sound (get sounds (rand-int (count sounds)))]
    (engine/play-sound sound)))

(defn play-player-shot []
  (engine/play-sound (:player-shot-sound @s/context)))
