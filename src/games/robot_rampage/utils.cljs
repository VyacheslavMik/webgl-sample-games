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
  (let [texture (js/PIXI.Texture.fromImage (texture "sprite_sheet.png"))
        rect (js/PIXI.Rectangle. (:x frame-rect)
                                 (:y frame-rect)
                                 (:w frame-rect)
                                 (:h frame-rect))
        pixi-sprite (js/PIXI.Sprite. (js/PIXI.Texture. texture rect rect))]
    (when center?
      (.. pixi-sprite -anchor (set 0.5)))
    (.. container (addChild pixi-sprite))
    pixi-sprite))

(defn sprite-center [sprite]
  {:x (+ (-> sprite :location :x) (/ (:frame-width sprite) 2))
   :y (+ (-> sprite :location :y) (/ (:frame-height sprite) 2))})

(defn update-pixi-sprite [sprite]
  (when-let [pixi-sprite (:sprite sprite)]
    (let [{:keys [x y]} (sprite-center sprite)
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
      (.. pixi-sprite -position (set x y)))))
