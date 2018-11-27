(ns games.robot-rampage.utils)

(defn random-int [min max]
  (+ (rand-int (- max min)) min))

(defn clamp [v min-v max-v]
  (min max-v (max v min-v)))

(defn vector-sub [v1 v2]
  (-> v1
      (update :x - (:x v2))
      (update :y - (:y v2))))

(def rectangle-left :x)
(def rectangle-top  :y)
