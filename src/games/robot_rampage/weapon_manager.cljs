(ns games.robot-rampage.weapon-manager
  (:require [games.engine :as engine]
            [games.robot-rampage.storage :as s]
            [games.robot-rampage.utils :as u]
            [games.robot-rampage.sprite :as sprite]
            [games.robot-rampage.tile-map :as tile-map]
            [games.robot-rampage.effects-manager :as effects-manager]
            [games.robot-rampage.path-finder :as path-finder]
            [games.robot-rampage.sound-manager :as sound-manager]
            [games.robot-rampage.particle :as particle]))

(def weapon-speed 600)

(def rocket-min-timer 0.5)
(def shot-min-timer 0.15)

(def shot-rectangle {:x 0 :y 128 :w 32 :h 32})

(def time-between-powerups 2)

(def max-active-powerups 5)
(def weapon-time-default 30)
(def triple-weapon-split-angle (* 15 (/ Math/PI 180)))

(defn init []
  (swap! s/context assoc :weapon-manager
         {:shot-timer 0
          :shots []
          :powerups []
          :time-since-last-powerup 0
          :weapon-time-remaining 0 
          :current-weapon-type :normal}))

(defn weapon-fire-delay []
  (if (= (get-in @s/context [:weapon-manager :current-weapon-type]) :rocket)
    rocket-min-timer
    shot-min-timer))

(defn can-fire-weapon []
  (>= (get-in @s/context [:weapon-manager :shot-timer]) (weapon-fire-delay)))

(defn add-shot [location velocity frame]
  (let [shot (-> (particle/new-particle location
                                        shot-rectangle
                                        velocity
                                        {:x 0 :y 0}
                                        400
                                        120
                                        [1 1 1 1]
                                        [1 1 1 1])
                 (sprite/add-frame (update shot-rectangle :x + (:w shot-rectangle)))
                 (assoc :animate? false)
                 (sprite/set-frame frame)
                 (sprite/rotate-to velocity))]
    (swap! s/context update-in [:weapon-manager :shots] conj shot)
    (sound-manager/play-player-shot)))

(defn fire-weapon [location velocity]
  (let [current-weapon-type (get-in @s/context [:weapon-manager :current-weapon-type])]
    (case current-weapon-type
      :normal
      (add-shot location velocity 0)

      :triple
      (let [base-angle (Math/atan2 (:y velocity) (:x velocity))
            offset triple-weapon-split-angle]
        (add-shot location velocity 0)
        (add-shot location
                  (u/vector-mul
                   {:x (Math/cos (- base-angle offset))
                    :y (Math/sin (- base-angle offset))}
                   (u/vector-length velocity))
                  0)
        (add-shot location
                  (u/vector-mul
                   {:x (Math/cos (+ base-angle offset))
                    :y (Math/sin (+ base-angle offset))}
                   (u/vector-length velocity))
                  0))

      :rocket
      (add-shot location velocity 1)))
  (swap! s/context assoc-in [:weapon-manager :shot-timer] 0))

(defn create-large-explosion [location]
  (effects-manager/add-larget-explosion (u/vector-add location {:x -10 :y -10}))
  (effects-manager/add-larget-explosion (u/vector-add location {:x -10 :y 10}))
  (effects-manager/add-larget-explosion (u/vector-add location {:x 10 :y 10}))
  (effects-manager/add-larget-explosion (u/vector-add location {:x 10 :y -10}))
  (effects-manager/add-larget-explosion location))

(defn check-rocket-splash-damage [location]
  (let [rocket-splash-raidus 40]
    (doall
     (map-indexed (fn [i enemy]
                    (when-not (:destroyed? enemy)
                      (when (sprite/circle-colliding? (:enemy-base enemy)
                                                      location rocket-splash-raidus)
                        (swap! s/context assoc-in [:enemies i :destroyed?] true)
                        (swap! s/context update-in [:game-manager :score] + 10)
                        (sound-manager/play-explosion)
                        (effects-manager/add-explosion (sprite/world-center (:enemy-base enemy))
                                                       {:x 0 :y 0}))))
                  (:enemies @s/context)))))

(defn check-shot-wall-impacts [shot]
  (if (:expired? shot)
    shot
    (if (tile-map/wall-tile? (tile-map/get-square-at-pixel (sprite/world-center shot)))
      (do
        (if (= (:current-frame shot) 0)
          (effects-manager/add-sparks-effect (sprite/world-center shot) (:velocity shot))
          (do
            (create-large-explosion (sprite/world-center shot))
            (check-rocket-splash-damage (sprite/world-center shot))))
        (assoc shot :expired? true))
      shot)))

(defn check-shot-enemy-impacts [shot]
  (if (:expired? shot)
    shot
    (let [impacts? (some (fn [[i enemy]]
                           (when (and (not (:destroyed? enemy))
                                      (sprite/circle-colliding? shot
                                                                (sprite/world-center (:enemy-base enemy))
                                                                (:collision-radius (:enemy-base enemy))))
                             {:i i :enemy-base (:enemy-base enemy)}))
                         (map-indexed vector (:enemies @s/context)))]
      (if impacts?
        (do
          (swap! s/context assoc-in [:enemies (:i impacts?) :destroyed?] true)
          (swap! s/context update-in [:game-manager :score] + 10)
          (sound-manager/play-explosion)
          (if (= (:current-frame shot) 0)
            (effects-manager/add-explosion (sprite/world-center (:enemy-base impacts?))
                                           (u/vector-div (:velocity (:enemy-base impacts?)) 30))
            (when (= (:current-frame shot) 1)
              (create-large-explosion (sprite/world-center shot))
              (check-rocket-splash-damage (sprite/world-center shot))))
          (assoc shot :expired? true))
        shot))))

(defn pathing-node-position []
  (let [base-sprite (get-in @s/context [:player :base-sprite])]
    (tile-map/get-square-at-pixel (sprite/world-center base-sprite))))

(defn try-to-spawn-powerup [x y type]
  (let [powerups (get-in @s/context [:weapon-manager :powerups])]
    (when (< (count powerups) max-active-powerups)
      (let [this-destination (tile-map/square-world-rectangle x y)
            blocked? (some (fn [p] (= (sprite/world-rectangle p) this-destination)) powerups)
            path-found? (path-finder/find-path {:x x :y y} (pathing-node-position))]
        (when (and (not blocked?) path-found?)
          (let [frame (if (= type :rocket) 1 0)
                new-powerup (-> (sprite/new-sprite {:x (:x this-destination)
                                                    :y (:y this-destination)}
                                                   {:x 64 :y 128 :w 32 :h 32}
                                                   {:x 0 :y 0})
                                (assoc :animate? false
                                       :current-frame frame
                                       :collision-radius 14)
                                (sprite/add-frame {:x 96 :y 128 :w 32 :h 32}))
                powerups (conj powerups new-powerup)]
            (sprite/update-pixi-sprite new-powerup)
            (swap! s/context assoc-in [:weapon-manager :powerups] powerups)
            (swap! s/context assoc-in [:weapon-manager :time-since-last-powerup] 0)))))))

(defn check-powerup-spawns [elapsed]
  (swap! s/context update-in [:weapon-manager :time-since-last-powerup] + elapsed)
  (when (>= (get-in @s/context [:weapon-manager :time-since-last-powerup]) time-between-powerups)
    (let [type (if (= (rand-int 2) 1)
                 :rocket
                 :triple)]
      (try-to-spawn-powerup (rand-int tile-map/map-width)
                            (rand-int tile-map/map-height)
                            type))))

(defn check-powerup-pickups []
  (let [base-sprite (get-in @s/context [:player :base-sprite])
        current-weapon-type (atom nil)
        weapon-time-remaining (atom nil)
        powerups (->> (get-in @s/context [:weapon-manager :powerups])
                      (filterv (fn [powerup]
                                 (if (sprite/circle-colliding? base-sprite
                                                               (sprite/world-center powerup)
                                                               (:collision-radius powerup))
                                   (do
                                     (case (:current-frame powerup)
                                       0 (reset! current-weapon-type :triple)
                                       1 (reset! current-weapon-type :rocket))
                                     (reset! weapon-time-remaining weapon-time-default)
                                     (.. (:sprite powerup) destroy)
                                     false)
                                   true))))]
    (when @current-weapon-type
      (swap! s/context assoc-in [:weapon-manager :current-weapon-type] @current-weapon-type))
    (when @weapon-time-remaining
      (swap! s/context assoc-in [:weapon-manager :weapon-time-remaining] @weapon-time-remaining))
    (swap! s/context assoc-in [:weapon-manager :powerups] powerups)))

(defn check-weapon-upgrade-expire [elapsed]
  (when-not (= (get-in @s/context [:weapon-manager :current-weapon-type]) :normal)
    (swap! s/context update-in [:weapon-manager :weapon-time-remaining] - elapsed)
    (when (<= (get-in @s/context [:weapon-manager :weapon-time-remaining]) 0)
      (swap! s/context assoc-in [:weapon-manager :current-weapon-type] :normal))))

(defn destroy [shot]
  (when (:expired? shot)
    (when-let [sprite (:sprite shot)]
      (.. sprite  destroy)))
  shot)

(defn update* [elapsed]
  (swap! s/context update-in [:weapon-manager :shot-timer] + elapsed)
  (check-weapon-upgrade-expire elapsed)
  (let [shots (->> @s/context
                   :weapon-manager :shots
                   (mapv (fn [shot]
                           (-> shot
                               (particle/update* elapsed)
                               (check-shot-wall-impacts)
                               (check-shot-enemy-impacts))))
                   (remove (comp :expired? destroy))
                   (vec))]
    (swap! s/context assoc-in [:weapon-manager :shots] shots))
  (check-powerup-spawns elapsed)
  (check-powerup-pickups))
