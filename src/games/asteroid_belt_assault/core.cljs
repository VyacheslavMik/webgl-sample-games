(ns games.asteroid-belt-assault.core
  (:require [games.engine :as engine]))

(def context (atom {:state :title-screen}))

(def title-screen-delay-time 1)
(def player-starting-lives   3)

(def screen-width  800)
(def screen-height 600)

(defn make-star-field [star-count frame-rect]
  (let [texture (get-in @context [:textures :sprite-sheet])]
    (swap! context assoc :stars (mapv (fn [x]
                                        {:location {:x (rand-int screen-width)
                                                    :y (rand-int screen-height)}
                                         :texture texture
                                         :frames [frame-rect]
                                         :frame-width (:width frame-rect)
                                         :frame-height (:height frame-rect)})
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
                             (not= rotation 0) (assoc :effect {:type :rotate
                                                               :angle rotation})))
  ))

(defn draw* []
  (let [{:keys [textures state] :as ctx} @context]
    (when (= state :title-screen)
      (engine/draw-rectangle
       {:texture (:title-screen textures)}))

    (when (#{:playing :player-dead :game-over} state)
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

(defn update* [delta]
  (let [{:keys [state]} @context]
    (case state
      :title-screen
      (do
        (swap! context update :title-screen-timer + (* delta 0.001))
        (when (and (>= (:title-screen-timer @context) title-screen-delay-time)
                   (or (engine/key-pressed? :Space) (engine/get-touch-state)))
          (println state 1 2)
          (swap! context assoc-in [:player :lives-remaining] player-starting-lives)
          (swap! context assoc-in [:player :score] 0)
          ;; m_enemyManager.ResetDifficult();
          ;; ResetGame();
          (swap! context assoc :state :playing)))

      :playing
      (do
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
  (swap! context assoc-in [:textures :sprite-sheet] (engine/load-texture (texture "sprite_sheet.png"))))

