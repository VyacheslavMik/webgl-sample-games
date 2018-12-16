(ns games.gemstone-hunter.animation-strip
  (:require [goog.object]
            [games.gemstone-hunter.world :as world]))

(defn load-animation [path width]
  (let [base-texture (js/PIXI.BaseTexture.fromImage path true js/PIXI.SCALE_MODES.NEAREST)
        height (.. base-texture -height)
        frame-count (Math/floor (/ (.. base-texture -width) width))
        frames (to-array
                (for [i (range frame-count)]
                  (js/PIXI.Texture. base-texture
                                    (js/PIXI.Rectangle. (* i width) 0 width height)
                                    (js/PIXI.Rectangle. (* i width) 0 width height))))
        animation (js/PIXI.extras.AnimatedSprite. frames)]
    (.. animation -position (set -500 -500))
    (set! (.. animation -anchor -x) 0.5)
    animation))

(defn new-animation-strip [path frame-width name]
  (let [animation (load-animation path frame-width)]
    {:texture path
     :draw animation
     :frame-width frame-width
     :frame-timer 0
     :frame-delay 0.2
     :current-frame 0
     :loop-animation? true
     :finished-playing? false
     :next-animation nil
     :name name}))

(defn update-animation-strip [animation-strip]
  (let [sprite (:draw animation-strip)]
    (set! (.. sprite -loop)           (:loop-animation? animation-strip))
    (set! (.. sprite -animationSpeed) (:frame-delay animation-strip))
    animation-strip))

(defn play [animation-strip]
  (when-let [sprite (:draw animation-strip)]
    (world/add-interactive sprite)
    (.. sprite (gotoAndPlay 0)))
  (assoc animation-strip
         :current-frame 0
         :finished-playing? false))

(defn stop [animation-strip]
  (when-let [sprite (:draw animation-strip)]
    (world/remove-interactive sprite)
    (.. sprite stop))
  animation-strip)

(defn update* [animation-strip elapsed]
  (if (.. (:draw animation-strip) -playing)
    animation-strip
    (assoc animation-strip :finished-playing? true)))
