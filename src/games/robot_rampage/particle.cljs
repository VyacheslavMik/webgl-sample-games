(ns games.robot-rampage.particle
  (:require [games.engine :as engine]
            [games.robot-rampage.utils :as u]
            [games.robot-rampage.sprite :as sprite]))

(defn new-particle [location initial-frame velocity acceleration max-speed duration initial-color final-color]
  (assoc (sprite/new-sprite location initial-frame velocity)
         :initial-duration duration
         :remaining-duration duration
         :acceleration acceleration
         :max-speed max-speed
         :initial-color initial-color
         :final-color final-color))

(defn normalize-velocity [particle]
  (if (> (u/vector-length (:velocity particle)) (:max-speed particle))
    (let [vel (-> (:velocity particle)
                  (u/vector-normalize)
                  (u/vector-mul (:max-speed particle)))]
      (assoc particle :velocity vel))
    particle))

(defn duration-progress [particle]
  (/ (- (:initial-duration particle) (:remaining-duration particle)) (:initial-duration particle)))

(defn update* [particle elapsed]
  (let [expired? (<= (:remaining-duration particle) 0)]
    (cond-> particle
      true (assoc :expired? expired?)
      (not expired?) (->
                      (update :velocity u/vector-add (:acceleration particle))
                      (normalize-velocity)
                      (assoc :tint-color (u/color-lerp (:initial-color particle)
                                                       (:final-color particle)
                                                       (duration-progress particle)))
                      (update :remaining-duration dec))
      true (sprite/update* elapsed))))

(defn active? [particle]
  (> (:remaining-duration particle) 0))

(defn draw* [particle]
  (when (active? particle)
    (sprite/draw* particle)))
