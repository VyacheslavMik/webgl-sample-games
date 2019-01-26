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
            [games.robot-rampage.particle :as particle]
            [games.robot-rampage.effects-manager :as effects-manager]
            [games.robot-rampage.weapon-manager :as weapon-manager]
            [games.robot-rampage.enemy-manager :as enemy-manager]
            [games.robot-rampage.goal-manager :as goal-manager]
            [games.robot-rampage.tile-map :as tile-map]))

(def wave-complete-delay 6)
(def game-over-delay 6)

(defonce root         (u/fullscreen-container))
(defonce title-screen (u/fullscreen-sprite (u/texture "title_screen.png")))

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

(defonce game-over-text (text "G A M E  O V E R !"     {:x 400 :y 300} 36 false))
(defonce score-text     (text "Score: 0"               {:x 30  :y 5}   16 false true))
(defonce terminals-text (text "Terminals Remaining: 0" {:x 520 :y 5}   16 false true))
(defonce lives-text     (text "Lives Remaining: 0"     {:x 30  :y 20}  16 false true))
(defonce wave-text      (text "Beginning Wave 0"       {:x 400 :y 300} 28 false))

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
        (do
          (set! (.. game-over-text -visible) true)
          (swap! s/context assoc :state :game-over))
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
          (set! (.. lives-text -visible) true)
          (set! (.. score-text -visible) true)
          (set! (.. terminals-text -visible) true)
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
          (set! (.. wave-text -text) (str "Beginning Wave " (inc (get-in @s/context [:game-manager :current-wave]))))
          (set! (.. wave-text -visible) true)
          (swap! s/context assoc :state :wave-complete))
        (set! (.. score-text -text) (str "Score: " (get-in @s/context [:game-manager :score])))
        (set! (.. lives-text -text) (str "Lives Remaining: " (get-in @s/context [:player :lives-remaining])))
        (set! (.. terminals-text -text) (str "Terminals Remaining: " (get-in @s/context [:goal-manager :active-count]))))

      :wave-complete
      (do
        (swap! s/context update :wave-complete-timer + elapsed)
        (when (> (:wave-complete-timer @s/context) wave-complete-delay)
          (set! (.. wave-text -visible) false)
          (game-manager/start-new-wave)
          (swap! s/context assoc :state :playing)
          (swap! s/context assoc :wave-complete-timer 0)))

      :game-over
      (do
        (swap! s/context update :game-over-timer + elapsed)
        (when (> (:game-over-timer @s/context) game-over-delay)
          (set! (.. game-over-text -visible) false)
          (set! (.. camera/container -visible) false)
          (set! (.. title-screen -visible) true)
          (set! (.. lives-text -visible) false)
          (set! (.. score-text -visible) false)
          (set! (.. terminals-text -visible) false)
          (swap! s/context assoc :state :title-screen)
          (swap! s/context assoc :game-over-timer 0)))

      nil)))

(defn sound [sound-name]
  (str "sounds/robot_rampage/" sound-name))

(defn init []
  (swap! s/context assoc :state :title-screen)
  (swap! s/context assoc :wave-complete-timer 0)
  (swap! s/context assoc :game-over-timer 0)

  (swap! s/context assoc-in [:player-shot-sound] (sound "shot1.wav"))

  (swap! s/context assoc :difficult-timer 0)

  (swap! s/context assoc-in [:explosion-sounds] (mapv (fn [i] (sound (str "explosion" i ".wav")))
                                                      (range 1 5)))

  (when-not (:initialized? @s/context)
    (.. root (addChild title-screen))
    (.. root (addChild camera/container))
    (.. camera/container (addChild tile-map/container))
    (.. camera/container (addChild camera/screen))
    (.. camera/container (addChild particle/container))

    (.. root (addChild game-over-text))
    (.. root (addChild score-text))
    (.. root (addChild lives-text))
    (.. root (addChild terminals-text))
    (.. root (addChild wave-text))

    (set! (.. camera/container -visible) false)

    (player/init)
    (effects-manager/init)
    (weapon-manager/init)
    (camera/init)
    (goal-manager/init)
    (enemy-manager/init)
    (game-manager/init)

    (game/run (pixi/init
               [(u/texture "title_screen.png")
                (u/texture "sprite_sheet.png")]
               #()) update* root)
    (swap! s/context assoc :initialized? true)))
