(ns games.asteroid-belt-assault.core
  (:require [games.engine :as engine]))

(def context (atom {:state :title-screen}))

(def title-screen-delay-time 1)
(def player-starting-lives   3)

(def screen-width  800)
(def screen-height 600)

(def star-colors [[255 255 255 255]
                  [255 255 0   255]
                  [245 222 179 255]
                  [245 245 245 255]
                  [47  79  79  255]])

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
             :tint-color engine/color-white})

(defn make-star-field [star-count frame-rect star-velocity]
  (let [texture (get-in @context [:textures :sprite-sheet])]
    (swap! context assoc :stars (mapv (fn [_]
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

(defn set-sprite-rotation [sprite v]
  (assoc sprite :rotation (mod v 360)))

(defn draw-sprite [sprite]
  (let [rotation (:rotation sprite)
        center {:x (+ (-> sprite :location :x) (/ (:frame-width sprite) 2))
                :y (+ (-> sprite :location :y) (/ (:frame-height sprite) 2))}
        tex-coords (get-in sprite [:frames (:current-frame sprite)])]
    (engine/draw-rectangle (cond-> {:texture (:texture sprite)
                                    :color (:tint-color sprite)
                                    :tex-coords tex-coords
                                    :origin center}
                             (and rotation (not= rotation 0)) (assoc :effect {:type :rotate
                                                                              :angle rotation})))))

(defn update-sprite [delta sprite]
  (let [elapsed (* delta 0.001)
        sprite (-> sprite
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
  (swap! context update :stars
         (fn [stars]
           (mapv
            (fn [star]
              (let [star (update-sprite delta star)]
                (if (> (-> star :location :y) screen-height)
                  (assoc star :location {:x (rand-int screen-width) :y 0})
                  star)))
            stars))))

(defn draw* []
  (let [{:keys [textures state] :as ctx} @context]
    (when (= state :title-screen)
      (engine/draw-rectangle
       {:texture (:title-screen textures)}))

    (when (#{:playing :player-dead :game-over} state)
      (draw-star-field)
      ;; m_starField.Draw(m_spriteBatch);
      ;; m_asteroidManager.Draw(m_spriteBatch);
      ;; m_playerManager.Draw(m_spriteBatch);
      ;; m_enemyManager.Draw(m_spriteBatch);
      ;; m_explosionManager.Draw(m_spriteBatch);

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
        ;; m_starField.Update(gameTime);
        ;; m_asteroidManager.Update(gameTime);
        ;; m_playerManager.Update(gameTime);
        ;; m_enemyManager.Update(gameTime);
        ;; m_explosionManager.Update(gameTime);
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

(defn init []
  (engine/init {:draw-fn   draw*
                :update-fn update*
                :show-fps? true})

  (swap! context assoc-in [:textures :title-screen] (engine/load-texture (texture "title_screen.png")))
  (swap! context assoc-in [:textures :sprite-sheet] (engine/load-texture (texture "sprite_sheet.png")))

  (make-star-field 200 {:x 0 :y 450 :w 2 :h 2} {:x 0 :y 30}))

