(ns games.robot-rampage.weapon-manager
  (:require [games.engine :as engine]
            [games.robot-rampage.storage :as s]
            [games.robot-rampage.sprite :as sprite]
            [games.robot-rampage.particle :as particle]))

(def weapon-speed 600)

(def rocket-min-timer 0.5)
(def shot-min-timer 0.15)

(def shot-rectangle {:x 0 :y 128 :w 32 :h 32})

(defn init []
  (swap! s/context assoc :weapon-manager
         {:shot-timer 0
          :shots []
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

(defn update* [elapsed]
  (swap! s/context update-in [:weapon-manager :shot-timer] + elapsed)
  (let [shots (->> @s/context
                   :weapon-manager :shots
                   (mapv (fn [shot] (particle/update* shot elapsed)))
                   (remove :expired?)
                   (vec))]
    (swap! s/context assoc-in [:weapon-manager :shots] shots)
    ))

(defn draw* []
  #_(println (get-in @s/context [:weapon-manager :shots]))
  (doseq [shot (get-in @s/context [:weapon-manager :shots])]
    (particle/draw* shot)))
