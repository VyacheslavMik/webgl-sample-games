(ns games.robot-rampage.core
  (:require [games.engine :as engine]
            [games.game :as game]
            [games.pixi :as pixi]
            [games.controls :as controls]
            [games.robot-rampage.storage :as s]
            [games.robot-rampage.utils :as u]
            [games.robot-rampage.game-manager :as game-manager]
            [games.robot-rampage.camera :as camera]
            [games.robot-rampage.player :as player]
            [games.robot-rampage.sprite :as sprite]
            [games.robot-rampage.effects-manager :as effects-manager]
            [games.robot-rampage.weapon-manager :as weapon-manager]
            [games.robot-rampage.enemy-manager :as enemy-manager]
            [games.robot-rampage.goal-manager :as goal-manager]
            [games.robot-rampage.tile-map :as tile-map]))

(def wave-complete-delay 6)
(def game-over-delay 6)

(defonce root         (u/fullscreen-container))
(defonce title-screen (u/fullscreen-sprite (u/texture "title_screen.png")))

(defn draw* []
  (let [{:keys [textures state player] :as ctx} @s/context]
    (when (= state :title-screen)
      (engine/draw-rectangle
       {:texture (:title-screen textures)
        :size {:width 800 :height 600}}))

    (when (#{:playing :wave-complete :game-over} state)
      (tile-map/draw*)
      (weapon-manager/draw*)
      (enemy-manager/draw*)
      (effects-manager/draw*)
      (goal-manager/draw*)
      (player/draw*)

      (engine/draw-text {:text (str "Score: " (get-in @s/context [:game-manager :score]))
                         :align :start
                         :position {:x 30 :y 10}})
      (engine/draw-text {:text (str "Terminals Remaining: " (get-in @s/context [:goal-manager :active-count]))
                         :align :start
                         :position {:x 520 :y 10}})
      (when (>= (get-in @s/context [:player :lives-remaining]) 0)
        (engine/draw-text {:text (str "Lives Remaining: " (get-in @s/context [:player :lives-remaining]))
                           :align :start
                           :position {:x 30 :y 25}})))

    (when (= state :wave-complete)
      (engine/draw-text {:text (str "Beginning Wave " (inc (get-in @s/context [:game-manager :current-wave])))
                         :scale 4
                         :position {:x 400 :y 300}}))

    (when (= state :game-over)
      (engine/draw-text {:text "G A M E  O V E R !"
                         :scale 5
                         :position {:x 400 :y 300}}))))

(defn check-player-death []
  (let [base-sprite (get-in @s/context [:player :base-sprite])
        player-destroyed? (some (fn [enemy]
                                  (sprite/circle-colliding? (:enemy-base enemy)
                                                            (sprite/world-center base-sprite)
                                                            (:collision-radius base-sprite)))
                                (:enemies @s/context))]
    (when player-destroyed?
      (swap! s/context update-in [:player :lives-remaining] dec)
      (if (= (get-in @s/context [:player :lives-remaining]) 0)
        (swap! s/context assoc :state :game-over)
        (game-manager/start-current-wave)))))

(defn update* [delta]
  (let [{:keys [state]} @s/context
        elapsed (* delta 0.001)]
    (case state
      :title-screen
      (do
        (when (or (controls/key-pressed? :Space) (controls/get-touch-state))
          (set! (.. title-screen      -visible) false)
          (set! (.. camera/container  -visible) true)
          (game-manager/start-new-game)
          (swap! s/context assoc :state :playing)))

      :playing
      (do
        (player/update*          elapsed)
        (weapon-manager/update*  elapsed)
        (enemy-manager/update*   elapsed)
        (effects-manager/update* elapsed)
        (goal-manager/update*    elapsed)
        (check-player-death)
        (when (= (get-in @s/context [:goal-manager :active-count]) 0)
          (swap! s/context assoc :state :wave-complete)))

      :wave-complete
      (do
        (swap! s/context update :wave-complete-timer + elapsed)
        (when (> (:wave-complete-timer @s/context) wave-complete-delay)
          (game-manager/start-new-wave)
          (swap! s/context assoc :state :playing)
          (swap! s/context assoc :wave-complete-timer 0)))

      :game-over
      (do
        (swap! s/context update :game-over-timer + elapsed)
        (when (> (:game-over-timer @s/context) game-over-delay)
          (swap! s/context assoc :state :title-screen)
          (swap! s/context assoc :game-over-timer 0)))

      nil)))

(defn sound [sound-name]
  (str "sounds/robot_rampage/" sound-name))

(defn init []
  #_(engine/init {:draw-fn   draw*
                :update-fn update*})

  (swap! s/context assoc :state :title-screen)
  (swap! s/context assoc :wave-complete-timer 0)
  (swap! s/context assoc :game-over-timer 0)

  #_(swap! s/context assoc-in [:textures :title-screen] (engine/load-texture (texture "title_screen.png")))
  #_(swap! s/context assoc-in [:textures :sprite-sheet] (engine/load-texture (texture "sprite_sheet.png")))

  (swap! s/context assoc-in [:player-shot-sound] (sound "shot1.wav"))

  (swap! s/context assoc :difficult-timer 0)

  (swap! s/context assoc-in [:explosion-sounds] (mapv (fn [i] (sound (str "explosion" i ".wav")))
                                                      (range 1 5)))

  (player/init)
  (effects-manager/init)
  (weapon-manager/init)
  (camera/init)
  (tile-map/init)
  (goal-manager/init)
  (enemy-manager/init)
  (game-manager/init)

  (when-not (:initialized? @s/context)
    (.. root (addChild title-screen))
    (.. root (addChild camera/container))
    #_(.. root (addChild game-over-text))

    ;; (.. game-screen (addChild score-text))
    ;; (.. game-screen (addChild lives-text))
    ;; (.. game-screen (addChild particle-container))

    (set! (.. camera/container -visible) false)

    (game/run (pixi/init
               [(u/texture "title_screen.png")
                (u/texture "sprite_sheet.png")]
               #()) update* root)
    (swap! s/context assoc :initialized? true)))
