(ns games.asteroid-belt-assault.core
  (:require [games.engine :as engine]))

(def context (atom {:state :title-screen}))

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

(def duration-count 120)
(def explosion-max-speed 30)

(def piece-speed-scale 6)
(def point-speed-min 15)
(def point-speed-max 30)

(defn color-mul [color f]
  (mapv (fn [c] (* c f)) color))

(def initial-color [1.0 0.3 0.0 1.0])
(def final-color [0 0 0 0])

(def min-shot-timer 0.2)

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

(defn star-color []
  (->> (get star-colors (rand-int (count star-colors)))
       (mapv (partial * (/ (+ (rand-int 50) 50) 100)))
       (engine/rgb-color)))

(def sprite {:location {:x 0 :y 0}
             :texture nil
             :frames []
             :frame-width 0
             :frame-height 0
             :velocity {:x 0 :y 0}
             :current-frame 0
             :frame-time 0.1
             :time-for-current-frame 0
             :tint-color engine/color-white
             :collision-radius 0})

(defn make-star-field [star-count frame-rect star-velocity]
  (let [texture (get-in @context [:textures :sprite-sheet])]
    (swap! context assoc :stars
           (mapv (fn [_]
                   (assoc sprite
                          :location {:x (rand-int screen-width)
                                     :y (rand-int screen-height)}
                          :texture texture
                          :frames [frame-rect]
                          :frame-width (:w frame-rect)
                          :frame-height (:h frame-rect)
                          :velocity star-velocity
                          :tint-color (star-color)))
                 (range star-count)))))

(defn make-asteroids [asteroid-count frame-rect asteroid-frames]
  (let [texture (get-in @context [:textures :sprite-sheet])
        frames (mapv (fn [x] (update frame-rect :x + (* (:w frame-rect) x)))
                     (range 1 asteroid-frames))]
    (swap! context assoc :asteroids
           (mapv (fn [_]
                   (assoc sprite
                          :location {:x -500 :y -500}
                          :texture texture
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
                                                 :texture texture
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
    

(defn set-sprite-rotation [sprite v]
  (assoc sprite :rotation (mod v 360)))

(defn sprite-center [sprite]
  {:x (+ (-> sprite :location :x) (/ (:frame-width sprite) 2))
   :y (+ (-> sprite :location :y) (/ (:frame-height sprite) 2))})

(defn box-colliding? [sprite rect]
  (rectangle-intersects? {:x (-> sprite :location :x)
                          :y (-> sprite :location :y)
                          :width (:frame-width sprite)
                          :height (:frame-height sprite)}
                         rect))

(defn circle-colliding? [{{x1 :x y1 :y} :location radius :collision-radius} {x2 :x y2 :y} other-radius]
  (let [distance (Math/sqrt (+ (* (- x2 x1) (- x2 x1))
                               (* (- y2 y1) (- y2 y1))))]
    (< distance (+ radius other-radius))))

(defn draw-sprite [sprite]
  (let [rotation (:rotation sprite)
        tex-coords (get-in sprite [:frames (:current-frame sprite)])]
    (engine/draw-rectangle (cond-> {:texture (:texture sprite)
                                    :color (:tint-color sprite)
                                    :tex-coords tex-coords
                                    :origin (sprite-center sprite)}
                             (and rotation (not= rotation 0)) (assoc :effect {:type :rotate
                                                                              :angle rotation})))))

(defn update-sprite [sprite elapsed]
  (let [sprite (-> sprite
                   (update :time-for-current-frame + elapsed)
                   (update-in [:location :x] + (* (-> sprite :velocity :x) elapsed))
                   (update-in [:location :y] + (* (-> sprite :velocity :y) elapsed)))]
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
        (engine/key-pressed? :KeyW) (vector-add {:x  0 :y -1})
        (engine/key-pressed? :KeyS) (vector-add {:x  0 :y  1})
        (engine/key-pressed? :KeyA) (vector-add {:x -1 :y  0})
        (engine/key-pressed? :KeyD) (vector-add {:x  1 :y  0}))
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
  (engine/play-sound url))

(defn new-shot [location velocity shot-speed]
  (let [texture (get-in @context [:textures :sprite-sheet])
        frame-rect {:x 0 :y 300 :w 5 :h 5}
        frame-count 4
        frames (mapv (fn [x] (update frame-rect :x + (* (:w frame-rect) x)))
                     (range 1 frame-count))]
    (assoc sprite
           :texture texture
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

(defn update-shots [shots elapsed]
  (->> shots
       (mapv (fn [shot] (update-sprite shot elapsed)))
       (filterv (fn [shot] (rectangle-intersects?
                           screen-bounds
                           (assoc (:location shot)
                                  :width (:frame-width shot)
                                  :height (:frame-height shot)))))))

(defn update-player [delta]
  (let [elapsed (* delta 0.001)]
    (swap! context update :player
           (fn [player]
             (let [sprite (-> (:sprite player)
                              (assoc :velocity (player-velocity player))
                              (update-sprite elapsed)
                              (impose-movement-limits (:area-limit player)))
                   fire-shot? (and (engine/key-pressed? :Space)
                                   (>= (+ (:shot-timer player) elapsed) min-shot-timer))
                   alive? (not (:destroyed? player))]
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
    (engine/play-sound sound)))

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

(defn check-collisions []
  (check-shot-to-asteroid-collisions)
  (when-not (get-in @context [:player :destroyed?])
    (check-asteroid-to-player-collisions)
    ))

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

(defn draw* []
  (let [{:keys [textures state] :as ctx} @context]
    (when (= state :title-screen)
      (engine/draw-rectangle
       {:texture (:title-screen textures)}))

    (when (#{:playing :player-dead :game-over} state)
      (draw-star-field)
      (draw-asteroids)
      (draw-player)
      ;; m_enemyManager.Draw(m_spriteBatch);
      (draw-explosions)

      ;; m_spriteBatch.DrawString(m_pericles14,
      ;;                          "Score: " + m_playerManager.PlayerScore.ToString(),
      ;;                          m_scoreLocation,
      ;;                          Color.White);

      ;; if (m_playerManager.LivesRemaining >= 0)
      ;; {
      ;;  m_spriteBatch.DrawString(m_pericles14,
      ;;                           "Ships Remaining: " + m_playerManager.LivesRemaining.ToString(),
      ;;                           m_livesLocation,
      ;;                           Color.White);
      ;;  }
      )

    (when (= state :game-over)
      )

    ))

(defn reset-game []
  )

(defn update* [delta]
  (let [{:keys [state]} @context]
    (case state
      :title-screen
      (do
        (swap! context update :title-screen-timer + (* delta 0.001))
        (when (and (>= (:title-screen-timer @context) title-screen-delay-time)
                   (or (engine/key-pressed? :Space) (engine/get-touch-state)))
          (swap! context assoc-in [:player :lives-remaining] player-starting-lives)
          (swap! context assoc-in [:player :score] 0)
          ;; m_enemyManager.ResetDifficult();
          (reset-game)
          (swap! context assoc :state :playing)))

      :playing
      (do
        (update-star-field delta)
        (update-asteroids delta)
        (update-player delta)
        ;; m_enemyManager.Update(gameTime);
        (update-explosions delta)
        (check-collisions)
        ;; m_collisionManager.CheckCollisions();
        
        ;; m_difficultTimer += (float)gameTime.ElapsedGameTime.TotalSeconds;

        ;; if (m_difficultTimer >= m_difficultIncreaseTime)
        ;; {
        ;;     m_enemyManager.IncreaseDifficult();
        ;;     m_difficultTimer = 0f;
        ;; }

        ;; if (m_playerManager.Destoyed)
        ;; {
        ;;     m_playerDeathTimer = 0f;
        ;;     m_enemyManager.Active = false;
        ;;     m_playerManager.LivesRemaining--;

        ;;     if (m_playerManager.LivesRemaining < 0)
        ;;     {
        ;;         m_gameState = GameStates.GameOver;
        ;;     }
        ;;     else
        ;;     {
        ;;         m_gameState = GameStates.PlayerDead;
        ;;     }
        ;; }
        ;; break;

        )

      :player-dead
      (do)

      :game-over
      (do)

      nil))
  )

(defn texture [tex-name]
  (str "textures/asteroid_belt_assault/" tex-name))

(defn sound [sound-name]
  (str "sounds/asteroid_belt_assault/" sound-name))

(defn init []
  (engine/init {:draw-fn   draw*
                :update-fn update*
                :show-fps? true})

  (swap! context assoc-in [:textures :title-screen] (engine/load-texture (texture "title_screen.png")))
  (swap! context assoc-in [:textures :sprite-sheet] (engine/load-texture (texture "sprite_sheet.png")))

  (make-star-field 200 {:x 0 :y 450 :w 2 :h 2} {:x 0 :y 30})
  (make-asteroids 10 {:x 0 :y 0 :w 50 :h 50} 20)
  (make-player {:x 0 :y 150 :w 50 :h 50} 3)
  (prepare-explosion {:x 0 :y 100 :w 50 :h 50} 3 {:x 0 :y 450 :w 2 :h 2})

  (swap! context assoc-in [:player :shot-sound] (sound "shot1.wav"))

  (swap! context assoc-in [:explosion-sounds] (mapv (fn [i] (sound (str "explosion" i ".wav")))
                                                    (range 1 5))))
