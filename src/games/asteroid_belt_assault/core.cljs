(ns games.asteroid-belt-assault.core
  (:require [games.game :as game]
            [games.controls :as controls]
            [games.pixi :as pixi]))

(def context (atom {:state :title-screen}))

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
  (str "textures/asteroid_belt_assault/" tex-name))  

(defonce root         (fullscreen-container))
(defonce game-screen  (fullscreen-container))
(defonce title-screen (fullscreen-sprite (texture "title_screen.png")))

(defn text [val {:keys [x y]} size visible? & [no-anchor?]]
  (let [text (js/PIXI.Text. val
                            #js{:fontFamily "Arial"
                                :fontSize size
                                :fill "white"})]
    (.. text -position (set x y))
    (when-not no-anchor?
      (.. text -anchor (set 0.5)))
    (set! (.. text -visible) visible?)
    text))

(def title-screen-delay-time 1)
(def player-starting-lives   3)

(def screen-width   800)
(def screen-height  600)
(def screen-padding 10)

(def star-colors [[255 255 255 255]
                  [255 255 0   255]
                  [245 222 179 255]
                  [245 245 245 255]
                  [47  79  79  255]])

(def asteroid-min-speed 60)
(def asteroid-max-speed 120)

(def screen-bounds {:x 0 :y 0 :width screen-width :height screen-height})
(def off-screen {:x -500 :y -500})
(def shot-to-asteroid-impact {:x 0 :y -20})

(def min-piece-count 3)
(def max-piece-count 6)
(def min-point-count 20)
(def max-point-count 30)
(def enemy-gun-offset {:x 25 :y 25})

(def duration-count 120)
(def explosion-max-speed 30)

(def piece-speed-scale 6)
(def point-speed-min 15)
(def point-speed-max 30)

(def player-start-location {:x 390 :y 550})
(def score-location {:x 20 :y 10})
(def lives-location {:x 20 :y 25})

(def min-ships-per-wave 5)
(def max-ships-per-wave 8)
(def ship-spawn-wait-timer 0.5)

(def enemy-point-value 100)

(def difficult-increase-time 10)

(defn color-mul [color f]
  (mapv (fn [c] (* c f)) color))

(def initial-color [1.0 0.3 0.0 1.0])
(def final-color [0 0 0 0])

(def min-shot-timer 0.2)

(def player-death-delay-time 10)

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

(defn make-hex [n1 n2 n3]
  (+ n3 (* n2 256) (* n1 65536)))

(defn star-color []
  (->> (get star-colors (rand-int (count star-colors)))
       (mapv (partial * (/ (+ (rand-int 50) 50) 100)))
       (apply make-hex)))

(def sprite {:location {:x 0 :y 0}
             :texture nil
             :frames []
             :frame-width 0
             :frame-height 0
             :velocity {:x 0 :y 0}
             :current-frame 0
             :frame-time 0.1
             :time-for-current-frame 0
             :tint-color 0xFFFFFF
             :collision-radius 0})

(defn pixi-sprite [frame-rect]
  (let [texture (js/PIXI.Texture.fromImage (texture "sprite_sheet.png"))
        rect (js/PIXI.Rectangle. (:x frame-rect)
                                 (:y frame-rect)
                                 (:w frame-rect)
                                 (:h frame-rect))
        pixi-sprite (js/PIXI.Sprite. (js/PIXI.Texture. texture rect rect))]
    (.. pixi-sprite -anchor (set 0.5))
    (.. game-screen (addChild pixi-sprite))
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

(defn make-star-field [star-count frame-rect star-velocity]
  (swap! context assoc :stars
         (mapv (fn [_]
                 (assoc sprite
                        :location {:x (rand-int screen-width)
                                   :y (rand-int screen-height)}
                        :alpha (* 1 (/ (+ (rand-int 50) 50) 100))
                        :sprite (pixi-sprite frame-rect)
                        :frames [frame-rect]
                        :frame-width (:w frame-rect)
                        :frame-height (:h frame-rect)
                        :velocity star-velocity
                        :tint-color (star-color)))
               (range star-count))))

(defn make-asteroids [asteroid-count frame-rect asteroid-frames]
  (let [frames (mapv (fn [x] (update frame-rect :x + (* (:w frame-rect) x)))
                     (range 1 asteroid-frames))]
    (swap! context assoc :asteroids
           (mapv (fn [_]
                   (assoc sprite
                          :location {:x -500 :y -500}
                          :sprite (pixi-sprite frame-rect)
                          :frames (vec (concat [frame-rect] frames))
                          :rotation (rand-int 360)
                          :collision-radius 15
                          :velocity {:x 0 :y 0}))
                 (range asteroid-count)))))

(defn make-player [frame-rect frame-count]
  (let [texture (get-in @context [:textures :sprite-sheet])
        frames (mapv (fn [x] (update frame-rect :x + (* (:w frame-rect) x)))
                     (range 1 frame-count))]
    (swap! context assoc :player {:sprite (assoc sprite
                                                 :location {:x 500 :y 500}
                                                 :sprite (pixi-sprite frame-rect)
                                                 :frames (vec (concat [frame-rect] frames))
                                                 :collision-radius 15)
                                  :shots []
                                  :score 0
                                  :destroyed? false
                                  :speed 160
                                  :gun-offset {:x -2.5 :y -18}
                                  :shot-timer 0
                                  :area-limit {:x 0
                                               :y (/ screen-height 2)
                                               :width screen-width
                                               :height (/ screen-height 2)}})))

(defn prepare-explosion [frame-rect piece-count point-rectangle]
  (let [texture (get-in @context [:textures :sprite-sheet])
        frames (mapv (fn [x] (update frame-rect :x + (* (:w frame-rect) x)))
                     (range piece-count))]
    (swap! context assoc :explosion {:texture texture
                                     :piece-rectangles frames
                                     :point-rectangle point-rectangle})))

(defn prepare-enemies [frame-rect frame-count]
  (let [texture (get-in @context [:textures :sprite-sheet])
        frames (mapv (fn [x] (update frame-rect :x + (* x (:w frame-rect))))
                     (range frame-count))]
    (swap! context assoc :enemy-data {:frames frames
                                      :collision-radius 15
                                      :texture texture})
    (swap! context assoc :path-waypoints [[{:x 850 :y 300}
                                           {:x -100 :y 300}]

                                          [{:x -50 :y 225}
                                           {:x 850 :y 225}]

                                          [{:x -100 :y 50}
                                           {:x 150 :y 50}
                                           {:x 200 :y 75}
                                           {:x 200 :y 125}
                                           {:x 150 :y 150}
                                           {:x 150 :y 175}
                                           {:x 200 :y 200}
                                           {:x 600 :y 200}
                                           {:x 850 :y 600}]

                                          [{:x 600 :y -100}
                                           {:x 600 :y 250}
                                           {:x 580 :y 275}
                                           {:x 500 :y 250}
                                           {:x 500 :y 200}
                                           {:x 450 :y 175}
                                           {:x 400 :y 150}
                                           {:x -100 :y 150}]])
    (swap! context assoc :enemies-active? false)
    (swap! context assoc :enemies [])
    (swap! context assoc :ship-shot-chance 0.2)
    (swap! context assoc :next-wave-min-timer 8)
    (swap! context assoc :wave-spawns {0 0
                                       1 0
                                       2 0
                                       3 0})))

(defn set-sprite-rotation [sprite v]
  (assoc sprite :rotation (mod v 360)))

(defn box-colliding? [sprite rect]
  (rectangle-intersects? {:x (-> sprite :location :x)
                          :y (-> sprite :location :y)
                          :width (:frame-width sprite)
                          :height (:frame-height sprite)}
                         rect))

(defn vector-distance [{x1 :x y1 :y} {x2 :x y2 :y}]
  (Math/sqrt (+ (* (- x2 x1) (- x2 x1))
                (* (- y2 y1) (- y2 y1)))))

(defn circle-colliding? [{loc1 :location radius :collision-radius} loc2 other-radius]
  (let [distance (vector-distance loc1 loc2)]
    (< distance (+ radius other-radius))))

(defn draw-sprite [sprite]
  (let [rotation (:rotation sprite)
        tex-coords (get-in sprite [:frames (:current-frame sprite)])]
    #_(engine/draw-rectangle (cond-> {:texture (:texture sprite)
                                    :color (:tint-color sprite)
                                    :tex-coords tex-coords
                                    :origin (sprite-center sprite)}
                             (and rotation (not= rotation 0)) (assoc :effect {:type :rotate
                                                                              :radians rotation})))))

(defn update-sprite [sprite elapsed]
  (let [sprite (-> sprite
                   (update :time-for-current-frame + elapsed)
                   (update-in [:location :x] + (* (-> sprite :velocity :x) elapsed))
                   (update-in [:location :y] + (* (-> sprite :velocity :y) elapsed)))]
    (update-pixi-sprite sprite)
    (if (>= (:time-for-current-frame sprite) (:frame-time sprite))
      (-> sprite
          (assoc :current-frame (mod (inc (:current-frame sprite)) (count (:frames sprite))))
          (assoc :time-for-current-frame 0))
      sprite)))

(defn draw-star-field []
  (doseq [star (:stars @context)]
    (draw-sprite star)))

(defn update-star-field [delta]
  (let [elapsed (* delta 0.001)]
    (swap! context update :stars
           (fn [stars]
             (mapv
              (fn [star]
                (let [star (update-sprite star elapsed)]
                  (if (> (-> star :location :y) screen-height)
                    (assoc star :location {:x (rand-int screen-width) :y 0})
                    star)))
              stars)))))

(defn draw-asteroids []
  (doseq [asteroid (:asteroids @context)]
    (draw-sprite asteroid)))

(defn asteroid-on-screen? [asteroid]
  (rectangle-intersects? (sprite-center asteroid)
                         {:x (- screen-padding)
                          :y (- screen-padding)
                          :width (+ screen-width screen-padding)
                          :height (+ screen-height screen-padding)}))

(defn random-location [asteroid asteroids]
  (loop [location {:x 0 :y 0}
         location-ok false
         try-count 0]
    (if location-ok
      location
      (let [location (case (rand-int 3)
                       0 {:x (- (:frame-width asteroid))
                          :y (rand-int screen-height)}

                       1 {:x screen-width
                          :y (rand-int screen-height)}

                       2 {:x (rand-int screen-width)
                          :y (- (:frame-height asteroid))})
            try-count (inc try-count)
            rect (assoc location
                        :width (:frame-width asteroid)
                        :height (:frame-height asteroid))
            location-ok (not (some #(box-colliding? % rect) asteroids))]
        (if (and (> try-count 5) (not location-ok))
          (recur {:x -500 :y -500} true try-count)
          (recur location location-ok try-count))))))

(defn random-velocity []
  (-> {:x (- (rand-int 101) 50) :y (- (rand-int 101) 50)}
      (vector-normalize)
      (update :x * (+ (rand-int (- asteroid-max-speed asteroid-min-speed)) asteroid-min-speed))
      (update :y * (+ (rand-int (- asteroid-max-speed asteroid-min-speed)) asteroid-min-speed))))

(defn vector-add [v1 v2]
  (-> v1
      (update :x + (:x v2))
      (update :y + (:y v2))))

(defn vector-sub [v1 v2]
  (-> v1
      (update :x - (:x v2))
      (update :y - (:y v2))))

(defn vector-div [v f]
  (-> v
      (update :x / f)
      (update :y / f)))

(defn vector-mul [v f]
  (-> v
      (update :x * f)
      (update :y * f)))

(defn vector-reflect [v normal]
  (let [val (* 2 (+ (* (:x v) (:x normal)) (* (:y v) (:y normal))))]
    (-> v
        (update :x - (* (:x normal) val))
        (update :y - (* (:y normal) val)))))

(defn bounce-asteroids [x y]
  (let [{velocity1 :velocity :as a1} (get-in @context [:asteroids x])
        {velocity2 :velocity :as a2} (get-in @context [:asteroids y])
        c-of-mass (vector-div (vector-add velocity1 velocity2) 2)
        center1 (sprite-center a1)
        center2 (sprite-center a2)
        normal1 (-> (vector-sub center2 center1) (vector-normalize))
        normal2 (-> (vector-sub center1 center2) (vector-normalize))
        velocity1 (-> velocity1
                      (vector-sub c-of-mass)
                      (vector-reflect normal1)
                      (vector-add c-of-mass))
        velocity2 (-> velocity2
                      (vector-sub c-of-mass)
                      (vector-reflect normal2)
                      (vector-add c-of-mass))]
    (swap! context assoc-in [:asteroids x :velocity] velocity1)
    (swap! context assoc-in [:asteroids y :velocity] velocity2)))

(defn update-asteroids [delta]
  (let [elapsed (* delta 0.001)]
    (swap! context update :asteroids
           (fn [asteroids]
             (mapv
              (fn [asteroid]
                (let [asteroid (update-sprite asteroid elapsed)]
                  (if (asteroid-on-screen? asteroid)
                    asteroid
                    (assoc asteroid
                           :location (random-location asteroid asteroids)
                           :velocity (random-velocity)))))
              asteroids)))
    (doseq [x (range (count (:asteroids @context)))]
      (doseq [y (range (inc x) (count (:asteroids @context)))]
        (when (circle-colliding? (get-in @context [:asteroids x])
                                 (sprite-center (get-in @context [:asteroids y]))
                                 (get-in @context [:asteroids y :collision-radius]))
          (bounce-asteroids x y))))))

(defn draw-player []
  (let [player (:player @context)]
    (when-not (:destroyed? player)
      (draw-sprite (:sprite player)))
    (doseq [shot (:shots player)]
      (draw-sprite shot))))

(defn player-velocity [player]
  (-> (cond-> {:x 0 :y 0}
        (controls/key-pressed? :KeyW) (vector-add {:x  0 :y -1})
        (controls/key-pressed? :KeyS) (vector-add {:x  0 :y  1})
        (controls/key-pressed? :KeyA) (vector-add {:x -1 :y  0})
        (controls/key-pressed? :KeyD) (vector-add {:x  1 :y  0}))
      (vector-normalize)
      (vector-mul (:speed player))))

(defn rectangle-right [rectangle]
  (+ (:x rectangle) (:width rectangle)))

(defn rectangle-bottom [rectangle]
  (+ (:y rectangle) (:height rectangle)))

(defn impose-movement-limits [sprite area-limit]
  (let [hw (/ (-> sprite :frames first :w) 2)
        hh (/ (-> sprite :frames first :h) 2)]
    (cond-> sprite
      (< (-> sprite :location :x) (+ (:x area-limit) hw))
      (assoc-in [:location :x] (+ (:x area-limit) hw))
      
      (> (-> sprite :location :x) (- (rectangle-right area-limit) hw))
      (assoc-in [:location :x] (- (rectangle-right area-limit) hw))

      (< (-> sprite :location :y) (+ (:y area-limit) hh))
      (assoc-in [:location :y] (+ (:y area-limit) hh))

      (> (-> sprite :location :y) (- (rectangle-bottom area-limit) hh))
      (assoc-in [:location :y] (- (rectangle-bottom area-limit) hh)))))

(defn play-player-shot [url]
  (game/play-sound url))

(defn new-shot [location velocity shot-speed]
  (let [frame-rect {:x 0 :y 300 :w 5 :h 5}
        frame-count 4
        frames (mapv (fn [x] (update frame-rect :x + (* (:w frame-rect) x)))
                     (range 1 frame-count))]
    (assoc sprite
           :sprite (pixi-sprite frame-rect)
           :location location
           :velocity (vector-mul velocity shot-speed)
           :frames (vec (concat [frame-rect] frames))
           :frame-height (:h frame-rect)
           :frame-width (:w frame-rect)
           :collision-radius 2)))

(defn fire-shot [player]
  (play-player-shot (:shot-sound player))
  (-> player
      (assoc :shot-timer 0)
      (update :shots conj (new-shot (vector-add (-> player :sprite :location)
                                                (:gun-offset player))
                                    {:x 0 :y -1}
                                    250))))

(defn enemy-fire-shot [enemy]
  (game/play-sound (:enemy-shot-sound @context))
  (let [player-center (sprite-center (get-in @context [:player :sprite]))
        fire-loc (-> enemy :sprite :location
                     (vector-add enemy-gun-offset))
        shot-direction (-> (vector-sub player-center fire-loc)
                           (vector-normalize))]
    (swap! context update :enemy-shots conj
           (new-shot fire-loc shot-direction 150))))

(defn update-shots [shots elapsed]
  (->> shots
       (mapv (fn [shot] (update-sprite shot elapsed)))
       (filterv (fn [shot]
                  (let [on-screen? (rectangle-intersects?
                                    screen-bounds
                                    (assoc (:location shot)
                                           :width (:frame-width shot)
                                           :height (:frame-height shot)))]
                    (when-not on-screen?
                      (.. (:sprite shot) destroy))
                    on-screen?)))))

(defn update-player [delta]
  (let [elapsed (* delta 0.001)]
    (swap! context update :player
           (fn [player]
             (let [sprite (-> (:sprite player)
                              (assoc :velocity (player-velocity player))
                              (update-sprite elapsed)
                              (impose-movement-limits (:area-limit player)))
                   fire-shot? (and (controls/key-pressed? :Space)
                                   (>= (+ (:shot-timer player) elapsed) min-shot-timer))
                   alive? (not (:destroyed? player))]
               (set! (.. (:sprite sprite) -visible) alive?)
               (cond-> player
                 alive?                  (update :shot-timer + elapsed)
                 (and fire-shot? alive?) (fire-shot)
                 true (update :shots (fn [shots] (update-shots shots elapsed)))
                 alive? (assoc :sprite sprite)))))))

(defn check-shot-to-asteroid-collisions []
  (let [shots (get-in @context [:player :shots])
        asteroids (get-in @context [:asteroids])]
    (doseq [i (range (count shots))]
      (let [shot (get shots i)]
        (doseq [j (range (count asteroids))]
          (let [asteroid (get asteroids j)]
            (when (circle-colliding? shot (sprite-center asteroid) (:collision-radius asteroid))
              (swap! context assoc-in [:player :shots i :location] off-screen)
              (swap! context update-in [:asteroids j :velocity] vector-add shot-to-asteroid-impact))))))))

(defn random-direction [scale]
  (loop [direction {:x (- (rand-int 101) 50) :y (- (rand-int 101) 50)}]
    (if (= (vector-length direction) 0)
      (recur {:x (- (rand-int 101) 50) :y (- (rand-int 101) 50)})
      (-> direction
          (vector-normalize)
          (vector-mul scale)))))

(defn random-int [min max]
  (+ (rand-int (- max min)) min))

(defn new-particle [location texture frame velocity]
  (assoc sprite
         :texture texture
         :location location
         :frames [frame]
         :frame-width (:w frame)
         :frame-height (:h frame)
         :velocity velocity
         :initial-duration duration-count
         :remaining-duration duration-count
         :acceleration {:x 0 :y 0}
         :max-speed explosion-max-speed
         :initial-color initial-color
         :final-color final-color))

(defn play-explosion []
  (let [sounds (:explosion-sounds @context)
        sound (get sounds (rand-int (count sounds)))]
    (game/play-sound sound)))

(defn add-explosion [location momentum]
  (let [piece-rectangles (get-in @context [:explosion :piece-rectangles])
        point-rectangle (get-in @context [:explosion :point-rectangle])
        texture (get-in @context [:explosion :texture])
        piece-location (vector-sub location {:x (/ (-> piece-rectangles first :w) 2)
                                             :y (/ (-> piece-rectangles first :h) 2)})
        pieces (random-int min-piece-count (inc max-piece-count))
        points (random-int min-point-count (inc max-point-count))
        explosion-particles (:explosion-particles @context)
        explosion-particles 
        (loop [x 0
               res explosion-particles]
          (if (< x pieces)
            (recur (inc x) (conj
                            res
                            (new-particle piece-location
                                          texture
                                          (get piece-rectangles (rand-int (count piece-rectangles)))
                                          (vector-add (random-direction piece-speed-scale) momentum))))
            res))

        explosion-particles 
        (loop [x 0
               res explosion-particles]
          (if (< x points)
            (recur (inc x) (conj
                            explosion-particles
                            (new-particle location
                                          texture
                                          point-rectangle
                                          (vector-add (random-direction (random-int point-speed-min
                                                                                    point-speed-max))
                                                      momentum))))
            res))]
    (swap! context assoc :explosion-particles explosion-particles)
    (play-explosion)))

(defn check-asteroid-to-player-collisions []
  (let [player (:player @context)
        asteroids (:asteroids @context)]
    (doseq [i (range (count asteroids))]
      (let [asteroid (get asteroids i)]
        (when (circle-colliding? asteroid
                                 (sprite-center (:sprite player))
                                 (-> player :sprite :collision-radius))
          (add-explosion (sprite-center asteroid) (vector-div (:velocity asteroid) 10))
          (swap! context assoc-in [:asteroids i :location] off-screen)
          (swap! context assoc-in [:player :destroyed?] true)
          (add-explosion (-> player :sprite sprite-center) {:x 0 :y 0}))))))

(defn check-shot-to-enemy-collisions []
  (let [shots (get-in @context [:player :shots])
        enemies (:enemies @context)]
    (doseq [i (range (count shots))]
      (let [shot (get shots i)]
        (doseq [j (range (count enemies))]
          (let [enemy (get enemies j)]
            (when (circle-colliding? shot (sprite-center (:sprite enemy)) (-> enemy :sprite :collision-radius))
              (swap! context assoc-in [:player :shots i :location] off-screen)
              (swap! context assoc-in [:enemies j :destroyed?] true)
              (swap! context update-in [:player :score] + enemy-point-value)
              (add-explosion (sprite-center (:sprite enemy))
                             (vector-div (-> enemy :sprite :velocity) 10)))))))))

(defn check-shot-to-player-collisions []
  (let [player (:player @context)
        shots (:enemy-shots @context)]
    (doseq [i (range (count shots))]
      (let [shot (get shots i)]
        (when (circle-colliding? shot (sprite-center (:sprite player)) (-> player :sprite :collision-radius))
          (swap! context assoc-in [:enemy-shots i :location] off-screen)
          (swap! context assoc-in [:player :destroyed?] true)
          (add-explosion (sprite-center (:sprite player)) {:x 0 :y 0}))))))

(defn check-enemy-to-player-collisions []
  (let [player (:player @context)
        enemies (:enemies @context)]
    (doseq [i (range (count enemies))]
      (let [enemy (get enemies i)]
        (when (circle-colliding? (:sprite enemy)
                                 (sprite-center (:sprite player))
                                 (-> player :sprite :collision-radius))
          (swap! context assoc-in [:enemies i :destroyed?] true)
          (add-explosion (sprite-center (:sprite enemy))
                         (-> enemy :sprite :velocity
                             (vector-div 10)))
          (swap! context assoc-in [:player :destroyed?] true)
          (add-explosion (sprite-center (:sprite player)) {:x 0 :y 0}))))))

(defn check-collisions []
  (check-shot-to-enemy-collisions)
  (check-shot-to-asteroid-collisions)
  (when-not (get-in @context [:player :destroyed?])
    (check-shot-to-player-collisions)
    (check-enemy-to-player-collisions)
    (check-asteroid-to-player-collisions)))

(defn particle-active? [particle]
  (> (:remaining-duration particle) 0))

(defn draw-explosions []
  (doseq [particle (:explosion-particles @context)]
    (when (particle-active? particle)
      (draw-sprite particle))))

(defn duration-progress [particle]
  (/ (- (:initial-duration particle) (:remaining-duration particle)) (:initial-duration particle)))

(defn color-lerp [[ar ag ab aa] [br bg bb ba] t]
  (let [t (max 0 (min t 1))]
    [(+ ar (* (- br ar) t))
     (+ ag (* (- bg ag) t))
     (+ ab (* (- bb ab) t))
     (+ aa (* (- ba aa) t))]))

(defn update-particle [particle elapsed]
  (if (particle-active? particle)
    (let [velocity (vector-add (:velocity particle) (:acceleration particle))
          velocity-length (vector-length velocity)
          velocity (if (> (vector-length velocity) (:max-speed particle))
                     (-> velocity
                         (vector-normalize)
                         (vector-mul (:max-speed particle)))
                     velocity)
          color (color-lerp (:initial-color particle)
                            (:final-color particle)
                            (duration-progress particle))]
      (-> particle
          (assoc :velocity velocity
                 :tint-color color)
          (update :remaining-duration dec)
          (update-sprite elapsed)))
    particle))

(defn update-explosions [delta]
  (let [elapsed (* delta 0.001)
        explosion-particles (->> (:explosion-particles @context)
                                 (mapv #(update-particle % elapsed))
                                 (filterv particle-active?))]
    (swap! context assoc :explosion-particles explosion-particles)))

(defn waypoint-reached? [enemy]
  (< (vector-distance (-> enemy :sprite :location) (:current-waypoint enemy))
     (/ (-> enemy :sprite :frame-width) 2)))

(defn enemy-active? [enemy]
  (if (:destroyed? enemy)
    false
    (if (> (count (:waypoints enemy)) 0)
      true
      (if (waypoint-reached? enemy)
        false
        true))))

(defn spawn-enemy [waypoints]
  (let [enemy-data (:enemy-data @context)]
    (swap! context update :enemies conj
           {:sprite (-> sprite
                        (merge enemy-data)
                        (assoc :sprite (pixi-sprite (-> enemy-data :frames first))
                               :frame-width (-> enemy-data :frames first :w)
                               :frame-height (-> enemy-data :frames first :h)))
            :previous-location (first waypoints)
            :speed 120
            :waypoints waypoints
            :current-waypoint (first waypoints)})))

(defn update-wave-spawn [elapsed]
  (swap! context update :ship-spawn-timer + elapsed)
  (when (> (:ship-spawn-timer @context) ship-spawn-wait-timer)
    (doseq [[id count] (:wave-spawns @context)]
      (when (> count 0)
        (spawn-enemy (get-in @context [:path-waypoints id]))
        (swap! context update-in [:wave-spawns id] dec)))
    (swap! context assoc :ship-spawn-timer 0))
  (swap! context update :next-wave-timer + elapsed)
  (when (> (:next-wave-timer @context) (:next-wave-min-timer @context))
    (let [id (rand-int (count (:path-waypoints @context)))]
      (swap! context assoc-in [:wave-spawns id] (random-int min-ships-per-wave (inc max-ships-per-wave))))
    (swap! context assoc :next-wave-timer 0)))

(defn update-enemy [enemy elapsed]
  (if (enemy-active? enemy)
    (let [heading (-> (vector-sub (:current-waypoint enemy)
                                  (-> enemy :sprite :location))
                      (vector-normalize)
                      (vector-mul (:speed enemy)))
          previous-location (-> enemy :sprite :location)
          sprite (-> (:sprite enemy)
                     (assoc :velocity heading)
                     (update-sprite elapsed))
          rotation (Math/atan2 (- (-> sprite :location :y) (:y previous-location))
                               (- (-> sprite :location :x) (:x previous-location)))
          sprite (assoc sprite :rotation rotation)
          enemy (->  enemy
                     (assoc :previous-location previous-location)
                     (assoc :sprite sprite))]
      (if (and (waypoint-reached? enemy) (> (count (:waypoints enemy)) 0))
        (-> enemy
            (assoc :current-waypoint (first (:waypoints enemy)))
            (assoc :waypoints (vec (rest (:waypoints enemy)))))
        enemy))
    enemy))

(defn destroy-enemy [enemy]
  (when-not (enemy-active? enemy)
    (.. (-> enemy :sprite :sprite) destroy))
  enemy)

(defn update-enemies [delta]
  (let [elapsed (* delta 0.001)
        ship-shot-chance (:ship-shot-chance @context)]
    (swap! context update :enemy-shots
           (fn [shots] (update-shots shots elapsed)))
    (let [enemies (->> (:enemies @context)
                       (mapv (fn [enemy] (update-enemy enemy elapsed)))
                       (filterv (comp enemy-active? destroy-enemy)))]
      (swap! context assoc :enemies enemies)
      (when-not (get-in @context [:player :destroyed?])
        (doseq [enemy enemies]
          (when (<= (/ (rand-int 1000) 10) ship-shot-chance)
            (enemy-fire-shot enemy)))))
    (when (:enemies-active? @context)
      (update-wave-spawn elapsed))))

(defn draw-enemy [enemy]
  (when (enemy-active? enemy)
    (draw-sprite (:sprite enemy))))

(defn draw-enemies []
  (doseq [shot (:enemy-shots @context)]
    (draw-sprite shot))
  (doseq [enemy (:enemies @context)]
    (draw-enemy enemy)))

(defn draw* []
  #_(let [{:keys [textures state player] :as ctx} @context]
    (when (= state :title-screen)
      (engine/draw-rectangle
       {:texture (:title-screen textures)}))

    (when (#{:playing :player-dead :game-over} state)
      (draw-star-field)
      (draw-asteroids)
      (draw-player)
      (draw-enemies)
      (draw-explosions)

      (engine/draw-text {:align :start
                         :text (str "Score: " (:score player))
                         :position score-location})
      (when (>= (:lives-remaining player) 0)
        (engine/draw-text {:align :start
                           :text (str "Ships Remaining: " (:lives-remaining player))
                           :position lives-location})))

    (when (= state :game-over)
      (engine/draw-text {:text "G A M E  O V E R !"
                         :position {:x (/ screen-width 2) :y (/ screen-height 2)}}))))

(defn reset-game []
  (swap! context assoc-in [:player :sprite :location] player-start-location)
  (swap! context update :asteroids
         (fn [asteroids]
           (mapv (fn [asteroid]
                   (assoc asteroid :location {:x -500 :y -500}))
                 asteroids)))
  (swap! context assoc :enemies [])
  (swap! context assoc :enemies-active? true)
  (swap! context assoc-in [:player :shots] [])
  (swap! context assoc :enemy-shots [])
  (swap! context assoc-in [:player :destroyed?] false))

(defn increase-difficult []
  (when (<= (:ship-shot-chance @context) 1)
    (swap! context update :ship-shot-chance + 0.05))
  (when (>= (:next-wave-min-timer @context) 4)
    (swap! context update :next-wave-min-timer - 0.5)))

(defn reset-difficult []
  (swap! context assoc :ship-shot-chance 0.2)
  (swap! context assoc :next-wave-min-timer 8))

(defn update* [delta]
  (let [{:keys [state]} @context]
    (case state
      :title-screen
      (do
        (swap! context update :title-screen-timer + (* delta 0.001))
        (when (and (>= (:title-screen-timer @context) title-screen-delay-time)
                   (or (controls/key-pressed? :Space) (controls/get-touch-state)))
          (set! (.. title-screen -visible) false)
          (set! (.. game-screen  -visible) true)
          (swap! context assoc-in [:player :lives-remaining] player-starting-lives)
          (swap! context assoc-in [:player :score] 0)
          (reset-difficult)
          (reset-game)
          (swap! context assoc :state :playing)))

      :playing
      (do
        (update-star-field delta)
        (update-asteroids delta)
        (update-player delta)
        (update-enemies delta)
        (update-explosions delta)
        (check-collisions)

        (swap! context update :difficult-timer + (* delta 0.001))

        (when (>= (:difficult-timer @context) difficult-increase-time)
          (increase-difficult)
          (swap! context assoc :difficult-timer 0))

        (when (get-in @context [:player :destroyed?])
          (swap! context assoc :player-death-timer 0)
          (swap! context assoc :enemies-active? false)
          (swap! context update-in [:player :lives-remaining] dec)

          (if (< (get-in @context [:player :lives-remaining]) 0)
            (swap! context assoc :state :game-over)
            (swap! context assoc :state :player-dead))))

      :player-dead
      (do
        (swap! context update :player-death-timer + (* delta 0.001))
        (update-star-field delta)
        (update-asteroids delta)
        (update-enemies delta)
        (update-player delta)
        (update-explosions delta)

        (when (>= (:player-death-timer @context) player-death-delay-time)
          (reset-game)
          (swap! context assoc :state :playing)))

      :game-over
      (do
        (swap! context update :player-death-timer + (* delta 0.001))
        (update-star-field delta)
        (update-asteroids delta)
        (update-enemies delta)
        (update-player delta)
        (update-explosions delta)

        (when (>= (:player-death-timer @context) player-death-delay-time)
          (swap! context assoc :state :title-screen)))

      nil)))

(defn sound [sound-name]
  (str "sounds/asteroid_belt_assault/" sound-name))

(defn init []
  #_(engine/init {:draw-fn   draw*
                :update-fn update*
                :show-fps? true})

  #_(swap! context assoc-in [:textures :title-screen] (engine/load-texture (texture "title_screen.png")))
  #_(swap! context assoc-in [:textures :sprite-sheet] (engine/load-texture (texture "sprite_sheet.png")))

  (make-star-field 200 {:x 0 :y 450 :w 2 :h 2} {:x 0 :y 30})
  (make-asteroids 10 {:x 0 :y 0 :w 50 :h 50} 20)
  (make-player {:x 0 :y 150 :w 50 :h 50} 3)
  (prepare-explosion {:x 0 :y 100 :w 50 :h 50} 3 {:x 0 :y 450 :w 2 :h 2})
  (prepare-enemies {:x 0 :y 200 :w 50 :h 50} 6)

  (swap! context assoc-in [:player :shot-sound] (sound "shot1.wav"))
  (swap! context assoc :enemy-shot-sound (sound "shot2.wav"))

  (swap! context assoc :difficult-timer 0)

  (swap! context assoc-in [:explosion-sounds] (mapv (fn [i] (sound (str "explosion" i ".wav")))
                                                    (range 1 5)))

  
  (when-not (:initialized? @context)
    (.. root (addChild title-screen))
    (.. root (addChild game-screen))

    (set! (.. game-screen -visible) false)

    (game/run (pixi/init
               [(texture "title_screen.png")
                (texture "sprite_sheet.png")]
               #()) update* root)
    (swap! context assoc :initialized? true)))
