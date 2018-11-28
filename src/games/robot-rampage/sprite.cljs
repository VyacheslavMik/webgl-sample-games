(ns games.robot-rampage.sprite
  (:require [games.engine :as engine]
            [games.robot-rampage.utils :as u]
            [games.robot-rampage.storage :as s]
            [games.robot-rampage.camera :as camera]))

(def sprite {:texture nil

             :expired? false
             :animate? true
             :animate-when-stopped? true

             :collidable? true
             :collision-radius 0
             :bounding-x-padding 0
             :bounding-y-padding 0

             :world-location {:x 0 :y 0}
             :velocity {:x 0 :y 0}

             :frames []

             :current-frame 0
             :frame-time 0.1
             :time-for-current-frame 0

             :tint-color engine/color-white

             :rotation 0

             :frame-width 0
             :frame-height 0})

(defn screen-location [sprite]
  (camera/transform-point (:world-location sprite)))

(defn world-rectangle [sprite]
  {:x (-> sprite :world-location :x)
   :y (-> sprite :world-location :y)
   :width (:frame-width sprite)
   :height (:frame-height sprite)})

(defn screen-rectangle [sprite]
  (camera/transform-rectangle (world-rectangle sprite)))

(defn relative-center [sprite]
  {:x (/ (:frame-width sprite) 2) :y (/ (:frame-height sprite) 2)})

(defn world-center [sprite]
  (u/vector-add (:world-location sprite) (relative-center sprite)))

(defn screen-center [sprite]
  (camera/transform-point (world-center sprite)))

(defn bounding-box-rect [{{x :x y :y} :world-location
                          w :frame-width
                          h :frame-height
                          xp :bounding-x-padding
                          yp :bounding-y-padding}]
  {:x (+ x xp) :y (+ y yp) :width (- w (* xp 2)) :height (- h (* yp 2))})

(defn new-sprite [world-location initial-frame velocity]
  (let [texture (get-in @s/context [:textures :sprite-sheet])]
    (assoc sprite
           :world-location world-location
           :texture texture
           :velocity velocity
           :frames [initial-frame]
           :frame-width (:w initial-frame)
           :frame-height (:h initial-frame))))

(defn colliding? [sprite other-box]
  (and (:collidable? sprite)
       (not (:expired? sprite))
       (u/rectangle-intersects? (bounding-box-rect sprite) other-box)))

(defn circle-colliding? [sprite other-center other-radius]
  (and (:collidable? sprite)
       (not (:expired? sprite))
       (< (u/vector-distance (world-center sprite) other-center)
          (+ (:collision-radius sprite) other-radius))))

(defn add-frame [sprite frame-rectangle]
  (update sprite :frames conj frame-rectangle))

(defn rotate-to [sprite direction]
  (assoc sprite :rotation (Math/atan2 (:y direction) (:x direction))))

(defn animate [sprite]
  (if (and (:animate? sprite)
           (or (:animate-when-stopped? sprite)
               (not (u/vector-zero (:velocity sprite)))))
    (assoc sprite
           :current-frame (mod (inc (:current-frame sprite)) (count (:frames sprite)))
           :time-for-current-frame 0)
    sprite))

(defn update* [sprite elapsed]
  (if-not (:expired? sprite)
    (-> sprite
        (update :time-for-current-frame + elapsed)
        (animate)
        (update :world-location u/vector-add (u/vector-mul (:velocity sprite) elapsed)))
    sprite))

(defn draw* [sprite]
  (when (and (not (:expired? sprite))
             (camera/object-visible? (world-rectangle sprite)))
    (engine/draw-rectangle (cond-> {:texture (:texture sprite)
                                    :origin (screen-center sprite)
                                    :color (:tint-color sprite)
                                    :tex-coords (get (:frames sprite) (:current-frame sprite))}
                             (and (:rotation sprite) (not= (:rotation sprite) 0))
                             (assoc :effect {:type :rotate
                                             :radians (:rotation sprite)})))))
