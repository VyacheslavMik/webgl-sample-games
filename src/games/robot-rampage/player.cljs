(ns games.robot-rampage.player
  (:require [games.engine :as engine]
            [games.robot-rampage.storage :as s]
            [games.robot-rampage.utils :as u]
            [games.robot-rampage.camera :as camera]
            [games.robot-rampage.tile-map :as tile-map]
            [games.robot-rampage.sprite :as sprite]))

(def frame-side 32)

(def base-initial-frame {:x 0 :y 64 :w frame-side :h frame-side})
(def base-frame-count 6)

(def turret-initial-frame {:x 0 :y 96 :w frame-side :h frame-side})
(def turret-frame-count 1)

(def world-location {:x 32 :y 32})

(def scroll-area {:x 150 :y 100 :width 500 :height 400})

(defn make-base-sprite []
  (let [sprite (sprite/new-sprite world-location base-initial-frame {:x 0 :y 0})]
    (-> (reduce (fn [sprite x]
                  (sprite/add-frame
                   sprite
                   (update base-initial-frame :x + (* frame-side x))))
                sprite (range 1 base-frame-count))
        (assoc :bounding-y-padding 4
               :bounding-y-padding 4
               :animate-when-stopped? false))))

(defn make-turret-sprite []
  (let [sprite (sprite/new-sprite world-location turret-initial-frame {:x 0 :y 0})]
    (-> (reduce (fn [sprite x]
                  (sprite/add-frame
                   sprite
                   (update turret-initial-frame :x + (* frame-side x))))
                sprite (range 1 turret-frame-count))
        (assoc :animate? false))))

(defn init []
  (let [frame-width (:w base-initial-frame)
        frame-height (:h base-initial-frame)]
    (swap! s/context assoc :player
           {:base-sprite (make-base-sprite)
            :turret-sprite (make-turret-sprite)
            :lives-remaining 5

            :base-angle {:x 0 :y 0}
            :turret-angle {:x 0 :y 0}
            :speed 90})))

(defn handle-keyboard-movement []
  (cond-> {:x 0 :y 0}
    (engine/key-pressed? :KeyW) (update :y dec)
    (engine/key-pressed? :KeyA) (update :x dec)
    (engine/key-pressed? :KeyS) (update :y inc)
    (engine/key-pressed? :KeyD) (update :x inc)))

(defn handle-mouse-shots [player]
  (if-let [mouse-state (engine/get-mouse-state)]
    (let [screen-location (sprite/screen-location (:turret-sprite player))
          mouse-loc {:x (:x mouse-state) :y (:y mouse-state)}]
      (u/vector-sub mouse-loc screen-location))
    {:x 0 :y 0}))

(defn check-tile-obstacles [move-angle player elapsed]
  (let [new-horizontal-location (-> player
                                    :base-sprite :world-location
                                    (u/vector-add {:x (* (:x move-angle) (:speed player) elapsed) :y 0}))
        new-vertical-location (-> player
                                  :base-sprite :world-location
                                  (u/vector-add {:x 0 :y (* (:y move-angle) (:speed player) elapsed)}))
        new-horizontal-rect {:x (:x new-horizontal-location)
                             :y (-> player :base-sprite :world-location :y)
                             :width (-> player :base-sprite :frame-width)
                             :height (-> player :base-sprite :frame-height)}
        new-vertical-rect {:x (-> player :base-sprite :world-location :x)
                           :y (:y new-vertical-location)
                           :width (-> player :base-sprite :frame-width)
                           :height (-> player :base-sprite :frame-height)}

        world-rectangle (-> player :base-sprite sprite/world-rectangle)

        {:keys [horiz-left-pixel horiz-right-pixel]}
        (cond
          (> (:x move-angle) 0) {:horiz-left-pixel (u/rectangle-right world-rectangle)
                                 :horiz-right-pixel (u/rectangle-right new-horizontal-rect)}
          (< (:x move-angle) 0) {:horiz-left-pixel (u/rectangle-left new-horizontal-rect)
                                 :horiz-right-pixel (u/rectangle-left world-rectangle)}
          :else {:horiz-left-pixel 0
                 :horiz-right-pixel 0})

        {:keys [vert-top-pixel vert-bottom-pixel]}
        (cond
          (> (:y move-angle) 0) {:vert-top-pixel (u/rectangle-bottom world-rectangle)
                                 :vert-bottom-pixel (u/rectangle-bottom new-vertical-rect)}
          (< (:y move-angle) 0) {:vert-top-pixel (u/rectangle-top new-vertical-rect)
                                 :vert-bottom-pixel (u/rectangle-top world-rectangle)}
          :else {:vert-top-pixel 0
                 :vert-bottom-pixel 0})
        block-x? (if (= (:x move-angle) 0)
                   true
                   (some (fn [x]
                           (some (fn [y]
                                   (tile-map/wall-tile-by-pixel? {:x x :y (+ (:y new-horizontal-location) y)}))
                                 (range (-> player :base-sprite :frame-height))))
                         (range (Math/floor horiz-left-pixel) (Math/floor horiz-right-pixel))))
        block-y? (if (= (:y move-angle) 0)
                   true
                   (some (fn [y]
                           (some (fn [x]
                                   (tile-map/wall-tile-by-pixel? {:x (+ (:x new-vertical-location) x) :y y}))
                                 (range (-> player :base-sprite :frame-width))))
                         (range (Math/floor vert-top-pixel) (Math/floor vert-bottom-pixel))))]
    (cond-> move-angle
      block-x? (assoc :x 0)
      block-y? (assoc :y 0))))

(defn reposition-camera [move-angle player elapsed]
  (let [move-scale (* (:speed player) elapsed)
        screen-rectangle (sprite/screen-rectangle (:base-sprite player))]
    (when (and (< (:x screen-rectangle) (:x scroll-area))
               (< (:x move-angle) 0))
      (camera/move {:x (* (:x move-angle) move-scale) :y 0}))

    (when (and (> (u/rectangle-right screen-rectangle) (u/rectangle-right scroll-area))
               (> (:x move-angle) 0))
      (camera/move {:x (* (:x move-angle) move-scale) :y 0}))

    (when (and (< (:y screen-rectangle) (:y scroll-area))
               (< (:y move-angle) 0))
      (camera/move {:x 0 :y (* (:y move-angle) move-scale)}))

    (when (and (> (u/rectangle-bottom screen-rectangle) (u/rectangle-bottom scroll-area))
               (> (:y move-angle) 0))
      (camera/move {:x 0 :y (* (:y move-angle) move-scale)}))

    move-angle))

(defn update-angle [sprite angle]
  (if (u/vector-zero angle)
    sprite
    (sprite/rotate-to sprite angle)))

(defn handle-input [player elapsed]
  (let [move-angle (-> (handle-keyboard-movement)
                       (u/vector-normalize))
        velocity (-> move-angle
                     (check-tile-obstacles player elapsed)
                     (reposition-camera player elapsed)
                     (u/vector-mul (:speed player)))
        fire-angle (-> (handle-mouse-shots player)
                       (u/vector-normalize))]
    (-> player
        (update :base-sprite update-angle move-angle)
        (update :turret-sprite update-angle fire-angle)
        (assoc-in [:base-sprite :velocity] velocity))))

(defn clamp-to-world [player]
  (let [current-x (-> player :base-sprite :world-location :x)
        current-y (-> player :base-sprite :world-location :y)
        world-rectangle (camera/world-rectangle)
        current-x (u/clamp current-x 0
                           (- (u/rectangle-right world-rectangle)
                              (-> player :base-sprite :frame-width)))
        current-y (u/clamp current-y 0
                           (- (u/rectangle-bottom world-rectangle)
                              (-> player :base-sprite :frame-height)))]
    (assoc-in player [:base-sprite :world-location] {:x current-x :y current-y})))

(defn update-turret-sprite [player]
  (assoc-in player [:turret-sprite :world-location] (-> player :base-sprite :world-location)))

(defn update* [elapsed]
  (let [player (-> @s/context :player
                   (handle-input elapsed)
                   (update :base-sprite sprite/update* elapsed)
                   (clamp-to-world)
                   (update-turret-sprite))]
    (swap! s/context assoc :player player)))

(defn draw* []
  (let [player (:player @s/context)]
    (sprite/draw* (:base-sprite player))
    (sprite/draw* (:turret-sprite player))))
