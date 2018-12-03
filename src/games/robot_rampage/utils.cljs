(ns games.robot-rampage.utils)

(defn random-int [min max]
  (+ (rand-int (- max min)) min))

(defn clamp [v min-v max-v]
  (min max-v (max v min-v)))

(defn vector-sub [v1 v2]
  (-> v1
      (update :x - (:x v2))
      (update :y - (:y v2))))

(defn vector-add [v1 v2]
  (-> v1
      (update :x + (:x v2))
      (update :y + (:y v2))))

(defn vector-zero [v]
  (= (:x v) (:y v) 0))

(defn vector-mul [v f]
  (-> v
      (update :x * f)
      (update :y * f)))

(defn vector-div [v f]
  (-> v
      (update :x / f)
      (update :y / f)))

(defn vector-distance [{x1 :x y1 :y} {x2 :x y2 :y}]
  (Math/sqrt (+ (* (- x2 x1) (- x2 x1))
                (* (- y2 y1) (- y2 y1)))))

(defn vector-length [{:keys [x y]}]
  (Math/sqrt (+ (* x x) (* y y))))

(defn vector-normalize [{:keys [x y] :as v}]
  (if (= x y 0)
    v
    (let [norm (vector-length v)]
      {:x (/ x norm) :y (/ y norm)})))

(defn rectangle-intersects? [{x1 :x y1 :y w1 :width h1 :height} {x2 :x y2 :y w2 :width h2 :height}]
  (not
   (or
    (< (+ x1 w1) x2) (< (+ x2 w2) x1)
    (< (+ y1 h1) y2) (< (+ y2 h2) y1))))

(def rectangle-left :x)
(def rectangle-top  :y)

(defn rectangle-right [rectangle]
  (+ (:x rectangle) (:width rectangle)))

(defn rectangle-bottom [rectangle]
  (+ (:y rectangle) (:height rectangle)))

(defn rectangle-offset [rectangle point]
  (vector-add rectangle point))

(defn color-lerp [a b t]
  (let [ar (goog.object/get a "r") ag (goog.object/get a "g")
        ab (goog.object/get a "b") aa (goog.object/get a "a")
        br (goog.object/get b "r") bg (goog.object/get b "g")
        bb (goog.object/get b "b") ba (goog.object/get b "a")
        t (max 0 (min t 1))]
    #js {:r (+ ar (* (- br ar) t))
         :g (+ ag (* (- bg ag) t))
         :b (+ ab (* (- bb ab) t))
         :a (+ aa (* (- ba aa) t))}))
