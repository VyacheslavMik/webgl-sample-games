(ns games.robot-rampage.game-manager
  (:require [games.robot-rampage.storage :as s]
            [games.robot-rampage.tile-map :as tile-map]
            [games.robot-rampage.goal-manager :as goal-manager]))

(def max-terminal-count 15)
(def player-start-loc {:x 32 :y 32})

(defn init []
  (swap! s/context assoc :game-manager
         {:score 0
          :current-wave 0
          :current-terminal-count 8}))

(defn start-wave [wave]
  (when (> wave (get-in @s/context [:game-manager :current-wave]))
    (swap! s/context assoc-in [:game-manager :current-wave] wave)
    (when (< (get-in @s/context [:game-manager :current-terminal-count]) max-terminal-count)
      (swap! s/context update-in [:game-manager :current-terminal-count] inc))
    (tile-map/generate-random-map)
    (goal-manager/generate-computers (get-in @s/context [:game-manager :current-terminal-count])))
  (swap! s/context assoc-in [:player :base-sprite :world-location] player-start-loc)
  (swap! s/context assoc-in [:camera :position] {:x 0 :y 0})
  (swap! s/context assoc-in [:weapon-manager :current-weapon-type] :normal)
  (swap! s/context assoc-in [:weapon-manager :shots] [])
  (swap! s/context assoc-in [:weapon-manager :powerups] [])
  (swap! s/context assoc-in [:effects-manager :effects] [])
  (swap! s/context assoc :enemies []))

(defn start-new-wave []
  (start-wave (inc (get-in @s/context [:game-manager :current-wave]))))

(defn start-current-wave []
  (start-wave (get-in @s/context [:game-manager :current-wave])))

(defn start-new-game []
  (swap! s/context assoc-in [:player :lives-remaining] 5)
  (swap! s/context assoc-in [:game-manager :current-wave] 0)
  (swap! s/context assoc-in [:game-manager :score] 0)
  (start-new-wave))
