(ns games.robot-rampage.goal-manager
  (:require [games.robot-rampage.storage :as s]
            [games.robot-rampage.sprite :as sprite]
            [games.robot-rampage.utils :as u]
            [games.robot-rampage.tile-map :as tile-map]
            [games.robot-rampage.path-finder :as path-finder]
            [games.robot-rampage.computer-terminal :as computer-terminal]))

(def min-distance-from-player 250)

(defn init []
  (swap! s/context assoc :goal-manager
         {:active-count 0
          :computer-terminals []
          :initial-active-frame {:x 0 :y (* 7 32) :w 32 :h 32}
          :initial-disable-frame {:x (* 3 32) :y (* 7 32) :w 32 :h 32}
          :active-frame-count 3
          :disable-frame-count 3}))

(defn terminal-in-square [map-location]
  (some (fn [terminal]
          (= (:map-location terminal) map-location))
        (get-in @s/context [:goal-manager :computer-terminals])))

(defn detect-shutdowns []
  (let [player-base-sprite (get-in @s/context [:player :base-sprite])
        player-base-sprite-world-center (sprite/world-center player-base-sprite)
        terminals (->> (get-in @s/context [:goal-manager :computer-terminals])
                       (mapv (fn [terminal]
                               (if (computer-terminal/circle-colliding? terminal
                                                                        player-base-sprite-world-center
                                                                        (:collision-radius player-base-sprite))
                                 (do
                                   (swap! s/context update-in [:goal-manager :active-count] dec)
                                   (swap! s/context update-in [:game-manager :score] + 150)
                                   (computer-terminal/deactivate terminal))
                                 terminal))))]
    (swap! s/context assoc-in [:goal-manager :computer-terminals] terminals)))

(defn add-computer-terminal []
  (let [start-x (u/random-int 2 (- tile-map/map-width 2))
        start-y (u/random-int 0 (- tile-map/map-height 2))
        tile-location {:x start-x :y start-y}]
    (when (and (not (terminal-in-square tile-location))
               (not (tile-map/wall-tile? tile-location)))
      (let [player-base-sprite-world-center (sprite/world-center (get-in @s/context [:player :base-sprite]))]
        (when (>= (u/vector-distance (tile-map/get-square-center start-x start-y)
                                     player-base-sprite-world-center)
                  min-distance-from-player)
          (let [path (path-finder/find-path
                      tile-location
                      (tile-map/get-square-at-pixel player-base-sprite-world-center))]
            (when path
              (let [{:keys [initial-active-frame active-frame-count
                            initial-disable-frame]} (:goal-manager @s/context)
                    square-rect (tile-map/square-world-rectangle start-x start-y)
                    active-sprite (sprite/new-sprite {:x (:x square-rect) :y (:y square-rect)}
                                                     initial-active-frame
                                                     {:x 0 :y 0})
                    active-sprite (reduce (fn [acc x]
                                            (sprite/add-frame acc (update
                                                                   initial-active-frame
                                                                   :x + (* x (:w initial-active-frame)))))
                                          active-sprite (range active-frame-count))
                    active-sprite (assoc active-sprite :collision-radius 15)
                    disable-sprite (sprite/new-sprite {:x (:x square-rect) :y (:y square-rect)}
                                                      initial-disable-frame
                                                      {:x 0 :y 0})
                    terminal (computer-terminal/new-computer-terminal active-sprite disable-sprite tile-location)
                    time-offset (u/random-int 1 100)
                    terminal (assoc terminal :last-spawn-counter (/ time-offset 100))]
                (swap! s/context update-in [:goal-manager :computer-terminals] conj terminal)
                (swap! s/context update-in [:goal-manager :active-count] inc)))))))))

(defn generate-computers [computer-count]
  (doseq [terminal (-> @s/context :goal-manager :computer-terminals)]
    (.. (-> terminal :active-sprite   :sprite) destroy)
    (.. (-> terminal :inactive-sprite :sprite) destroy))
  (swap! s/context assoc-in [:goal-manager :computer-terminals] [])
  (loop []
    (when (< (get-in @s/context [:goal-manager :active-count]) computer-count)
      (add-computer-terminal)
      (recur))))

(defn update* [elapsed]
  (detect-shutdowns)
  (let [terminals (->> (get-in @s/context [:goal-manager :computer-terminals])
                       (mapv (fn [terminal]
                               (computer-terminal/update* terminal elapsed))))]
    (swap! s/context assoc-in [:goal-manager :computer-terminals] terminals)))
