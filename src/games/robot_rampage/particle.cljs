(ns games.robot-rampage.particle
  (:require [games.engine :as engine]
            [games.robot-rampage.utils :as u]
            [games.robot-rampage.sprite :as sprite]))

(defonce container (js/PIXI.particles.ParticleContainer. 1500 #js{:tint true}))

(defn new-particle [location initial-frame velocity acceleration max-speed duration initial-color final-color]
  (assoc (sprite/new-sprite location initial-frame velocity container)
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

(defn not-expired-particle-update [particle]
  (let [{:keys [tint alpha]} (u/color-lerp (:initial-color particle)
                                           (:final-color particle)
                                           (duration-progress particle))]
    (-> particle
     (update :velocity u/vector-add (:acceleration particle))
     (normalize-velocity)
     (assoc :tint-color tint
            :alpha alpha)
     (update :remaining-duration dec))))

(defn destroy [particle]
  (.. (:sprite particle) destroy)
  (dissoc particle :sprite))

(defn update* [particle elapsed]
  (let [expired? (<= (:remaining-duration particle) 0)]
    (cond-> particle
      true           (assoc :expired? expired?)
      (not expired?) (not-expired-particle-update)
      true           (sprite/update* elapsed)
      expired?       (destroy))))

(defn active? [particle]
  (> (:remaining-duration particle) 0))
