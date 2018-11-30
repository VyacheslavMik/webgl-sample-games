(ns games.robot-rampage.weapon-manager
  (:require [games.engine :as engine]
            [games.robot-rampage.storage :as s]
            [games.robot-rampage.sprite :as sprite]
            [games.robot-rampage.tile-map :as tile-map]
            [games.robot-rampage.effects-manager :as effects-manager]
            [games.robot-rampage.path-finder :as path-finder]
            [games.robot-rampage.particle :as particle]))

(def weapon-speed 600)

(def rocket-min-timer 0.5)
(def shot-min-timer 0.15)

(def shot-rectangle {:x 0 :y 128 :w 32 :h 32})

(def time-between-powerups 2)

(def max-active-powerups 5)

(defn init []
  (swap! s/context assoc :weapon-manager
         {:shot-timer 0
          :shots []
          :powerups []
          :time-since-last-powerup 0
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
                                        engine/color-white
                                        engine/color-white)
                 (sprite/add-frame (update shot-rectangle :x + (:w shot-rectangle)))
                 (assoc :animate? false)
                 (sprite/set-frame frame)
                 (sprite/rotate-to velocity))]
    (swap! s/context update-in [:weapon-manager :shots] conj shot)
    ;; sound manager play
    ))

(defn fire-weapon [location velocity]
  (let [current-weapon-type (get-in @s/context [:weapon-manager :current-weapon-type])]
    (case current-weapon-type
      :normal
      (add-shot location velocity 0)
      )
      

    )
  (swap! s/context assoc-in [:weapon-manager :shot-timer] 0)
  )

(defn check-shot-wall-impacts [shot]
  (if (:expired? shot)
    shot
    (if (tile-map/wall-tile? (tile-map/get-square-at-pixel (sprite/world-center shot)))
      (do
        (if (= (:current-frame shot) 0)
          (effects-manager/add-sparks-effect (sprite/world-center shot) (:velocity shot))
          ;; effect explosion
          )
        (assoc shot :expired? true))
      shot)))

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

(defn update* [elapsed]

  (swap! s/context update-in [:weapon-manager :shot-timer] + elapsed)
  (let [shots (->> @s/context
                   :weapon-manager :shots
                   (mapv (fn [shot]
                           (-> shot
                               (particle/update* elapsed)
                               (check-shot-wall-impacts))))
                   (remove :expired?)
                   (vec))]
    (swap! s/context assoc-in [:weapon-manager :shots] shots))
  (check-powerup-spawns elapsed))

(defn draw* []
  (let [{:keys [powerups shots]} (:weapon-manager @s/context)]
    (doseq [shot shots]
      (particle/draw* shot))
    (doseq [powerup powerups]
      (sprite/draw* powerup))))
