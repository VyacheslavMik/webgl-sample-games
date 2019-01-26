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

(defn color-lerp [[ar ag ab aa] [br bg bb ba] t]
  (let [t (max 0 (min t 1))]
    {:tint (js/PIXI.utils.rgb2hex #js[(+ ar (* (- br ar) t))
                                      (+ ag (* (- bg ag) t))
                                      (+ ab (* (- bb ab) t))])
     :alpha (+ aa (* (- ba aa) t))}))

(defn fullscreen-sprite [image]
  (let [sprite (js/PIXI.Sprite.
                (js/PIXI.Texture.fromImage image))]
    (set! (.. sprite -width) 800)
    (set! (.. sprite -height) 600)
    sprite))

(defn fullscreen-container []
  (let [container (js/PIXI.Container.)]
    (set! (.. container -width) 800)
    (set! (.. container -height) 600)
    container))

(defn texture [tex-name]
  (str "textures/robot_rampage/" tex-name))

(defn pixi-sprite [frame-rect container & [center?]]
  (let [texture (js/PIXI.Texture.fromImage (texture "sprite_sheet.png") true  js/PIXI.SCALE_MODES.NEAREST)
        rect (js/PIXI.Rectangle. (:x frame-rect)
                                 (:y frame-rect)
                                 (:w frame-rect)
                                 (:h frame-rect))
        pixi-sprite (js/PIXI.Sprite. (js/PIXI.Texture. texture rect rect))]
    (when center?
      (.. pixi-sprite -anchor (set 0.5)))
    (.. container (addChild pixi-sprite))
    pixi-sprite))
