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

             :tint-color 0xFFFFFF

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

(defn update-pixi-sprite [sprite]
  (when-let [pixi-sprite (:sprite sprite)]
    (let [{:keys [x y]} (world-center sprite)
          tex-coords (get-in sprite [:frames (:current-frame sprite)])
          rect (when (or (not= (:x tex-coords) (.. pixi-sprite -texture -frame -x))
                         (not= (:y tex-coords) (.. pixi-sprite -texture -frame -y))
                         (not= (:w tex-coords) (.. pixi-sprite -texture -frame -width))
                         (not= (:h tex-coords) (.. pixi-sprite -texture -frame -height)))
                 (js/PIXI.Rectangle. (:x tex-coords)
                                     (:y tex-coords)
                                     (:w tex-coords)
                                     (:h tex-coords)))]
      (when-let [rotation (:rotation sprite)]
        (set! (.. pixi-sprite -rotation) rotation))
      (when-let [alpha (:alpha sprite)]
        (set! (.. pixi-sprite -alpha) alpha))
      (when rect
        (set! (.. pixi-sprite -texture -orig) rect)
        (set! (.. pixi-sprite -texture -frame) rect)
        (.. pixi-sprite -texture (_updateUvs)))
      (set! pixi-sprite -tint (:tint-color sprite))
      (.. pixi-sprite -position (set x y))))
  sprite)

(defn new-sprite [world-location initial-frame velocity & [container]]
  (update-pixi-sprite (assoc sprite
                             :world-location world-location
                             :sprite (u/pixi-sprite initial-frame
                                                    (if container
                                                      container
                                                      camera/screen)
                                                    true)
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

(defn set-frame [sprite frame]
  (assoc sprite :current-frame (u/clamp frame 0 (-> sprite :frames count dec))))

(defn update* [sprite elapsed]
  (update-pixi-sprite sprite)
  (if-not (:expired? sprite)
    (-> sprite
        (update :time-for-current-frame + elapsed)
        (animate)
        (update :world-location u/vector-add (u/vector-mul (:velocity sprite) elapsed)))
    sprite))
