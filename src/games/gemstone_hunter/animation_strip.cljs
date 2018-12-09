(ns games.gemstone-hunter.animation-strip
  (:require [goog.object]))

(defn frame-count [animation-strip]
  (/ (goog.object/get (:texture animation-strip) "width") (:frame-width animation-strip)))

(defn frame-rectangle [animation-strip]
  {:x (* (:current-frame animation-strip) (:frame-width animation-strip))
   :y 0
   :w (:frame-width animation-strip)
   :h (goog.object/get (:texture animation-strip) "height")})

(defn new-animation-strip [texture frame-width name]
  {:texture texture
   :frame-width frame-width
   :frame-timer 0
   :frame-delay 0.05
   :current-frame 0
   :loop-animation? true
   :finished-playing? false
   :next-animation nil
   :name name})

(defn play [animation-strip]
  (assoc animation-strip
         :current-frame 0
         :finished-playing? false))

(defn check-frame [animation-strip]
  (if (>= (:current-frame animation-strip) (frame-count animation-strip))
    (if (:loop-animation? animation-strip)
      (assoc animation-strip :current-frame 0)
      (assoc animation-strip
             :current-frame (dec (frame-count animation-strip))
             :finished-playing? true))
    animation-strip))

(defn check-timer [animation-strip]
  (if (>= (:frame-timer animation-strip) (:frame-delay animation-strip))
    (-> animation-strip
        (update :current-frame inc)
        (assoc :frame-timer 0))
    animation-strip))

(defn update* [animation-strip elapsed]
  (-> animation-strip
      (update :frame-timer + elapsed)
      (check-timer)
      (check-frame)))
