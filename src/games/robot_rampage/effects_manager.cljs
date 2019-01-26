(ns games.robot-rampage.effects-manager
  (:require [games.robot-rampage.storage :as s]
            [games.robot-rampage.particle :as particle]
            [games.robot-rampage.utils :as u]))

(def color-yellow [1 1                  0 1])
(def color-orange [1 0.6470588235294118 0 1])

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

(defn add-explosion
  ([location
    momentum
    min-point-count
    max-point-count
    min-piece-count
    max-piece-count
    piece-speed-scale
    duration
    initial-color
    final-color]
   (let [{:keys [explosion-frames effects particle-frame]} (:effects-manager @s/context)
         explosion-max-speed 30
         point-speed-min (* piece-speed-scale 2)
         point-speed-max (* piece-speed-scale 3)
         piece-location (u/vector-sub location {:x (/ (-> explosion-frames first :w) 2)
                                                :y (/ (-> explosion-frames first :h) 2)})
         pieces (u/random-int min-piece-count (inc max-piece-count))

         effects (reduce (fn [acc x]
                           (conj acc (particle/new-particle piece-location
                                                            (get explosion-frames
                                                                 (rand-int (count explosion-frames)))
                                                            (u/vector-add
                                                             (random-direction piece-speed-scale)
                                                             momentum)
                                                            {:x 0 :y 0}
                                                            explosion-max-speed
                                                            duration
                                                            initial-color
                                                            final-color)))
                         effects (range pieces))

         points (u/random-int min-point-count (inc max-point-count))

         effects (reduce (fn [acc x]
                           (conj acc (particle/new-particle location
                                                            particle-frame
                                                            (u/vector-add
                                                             (random-direction (u/random-int point-speed-min
                                                                                             point-speed-max))
                                                             momentum)
                                                            {:x 0 :y 0}
                                                            explosion-max-speed
                                                            duration
                                                            initial-color
                                                            final-color)))
                         effects (range points))]
     (swap! s/context assoc-in [:effects-manager :effects] effects)))
  
  ([location momentum]
   (add-explosion location
                  momentum
                  15
                  20
                  2
                  4
                  6
                  90
                  [1.0 0.3 0 0.5]
                  [1.0 0.3 0 0])))

(defn add-larget-explosion [location]
  (add-explosion location
                 {:x 0 :y 0}
                 15
                 20
                 4
                 6
                 30
                 90
                 [1.0 0.3 0 0.5]
                 [1.0 0.3 0 0]))

(defn update-effects [effects elapsed]
  (let [c (count effects)]
    (loop [i 0
           acc (transient [])]
      (if (< i c)
        (let [effect (particle/update* (nth effects i) elapsed)]
          (recur (inc i) (if (:expired? effect) acc (conj! acc effect))))
        (persistent! acc)))))

(defn update* [elapsed]
  (let [effects (get-in @s/context [:effects-manager :effects])
        effects (update-effects effects elapsed)]
    (swap! s/context assoc-in [:effects-manager :effects] effects)))
