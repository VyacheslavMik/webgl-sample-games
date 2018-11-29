(ns games.robot-rampage.effects-manager
  (:require [games.engine :as engine]
            [games.robot-rampage.storage :as s]
            [games.robot-rampage.particle :as particle]
            [games.robot-rampage.utils :as u]))

(def color-yellow (engine/rgb-color [255 255 0 255]))
(def color-orange (engine/rgb-color [255 165 0 255]))

(defn init []
  (let [particle-frame {:x 0 :y 288 :w 2 :h 2}
        explosion-frame {:x 0 :y 256 :w 32 :h 32}
        explosion-frame-count 3]
  (swap! s/context assoc :effects-manager
         {:effects []
          :particle-frame particle-frame
          :explosion-frames (mapv (fn [x]
                                    (update explosion-frame :x + (* x (:w explosion-frame))))
                                  (range explosion-frame-count))})))

(defn random-direction [scale]
  (loop [direction {:x (- (rand-int 101) 50) :y (- (rand-int 101) 50)}]
    (if (= (u/vector-length direction) 0)
      (recur {:x (- (rand-int 101) 50) :y (- (rand-int 101) 50)})
      (-> direction
          (u/vector-normalize)
          (u/vector-mul scale)))))

(defn add-sparks-effect [location impact-velocity]
  (let [particle-count (u/random-int 10 20)
        particle-frame (get-in @s/context [:effects-manager :particle-frame])
        effects (get-in @s/context [:effects-manager :effects])
        effects (reduce (fn [acc x]
                          (conj acc (particle/new-particle (u/vector-sub location
                                                                         (u/vector-div impact-velocity 60))
                                                           particle-frame
                                                           (random-direction (u/random-int 10 20))
                                                           {:x 0 :y 0}
                                                           60
                                                           20
                                                           color-yellow
                                                           color-orange)))
                        effects (range particle-count))]
    (swap! s/context assoc-in [:effects-manager :effects] effects)))

(defn update* [elapsed]
  (let [effects (get-in @s/context [:effects-manager :effects])
        effects (->> effects
                     (mapv (fn [effect] (particle/update* effect elapsed)))
                     (remove :expired?)
                     (vec))]
    (swap! s/context assoc-in [:effects-manager :effects] effects)))

(defn draw* []
  (doseq [effect (get-in @s/context [:effects-manager :effects])]
    (particle/draw* effect)))
