(ns games.robot-rampage.core
  (:require [games.engine :as engine]
            [games.robot-rampage.storage :as s]
            [games.robot-rampage.game-manager :as game-manager]
            [games.robot-rampage.camera :as camera]
            [games.robot-rampage.player :as player]
            [games.robot-rampage.effects-manager :as effects-manager]
            [games.robot-rampage.weapon-manager :as weapon-manager]
            [games.robot-rampage.enemy-manager :as enemy-manager]
            [games.robot-rampage.tile-map :as tile-map]))

(defn draw* []
  (let [{:keys [textures state player] :as ctx} @s/context]
    (when (= state :title-screen)
      (engine/draw-rectangle
       {:texture (:title-screen textures)
        :size {:width 800 :height 600}}))

    (when (#{:playing :wave-complete :game-over} state)
      (tile-map/draw*)
      (weapon-manager/draw*)
      (player/draw*)
      (enemy-manager/draw*)
      (effects-manager/draw*))

    (when (= state :game-over))))

(defn update* [delta]
  (let [{:keys [state]} @s/context
        elapsed (* delta 0.001)]
    (case state
      :title-screen
      (do
        (when (or (engine/key-pressed? :Space) (engine/get-touch-state))
          (game-manager/start-new-game)
          (swap! s/context assoc :state :playing)))

      :playing
      (do
        (player/update*          elapsed)
        (weapon-manager/update*  elapsed)
        (enemy-manager/update*   elapsed)
        (effects-manager/update* elapsed))

      :player-dead
      (do)

      :game-over
      (do)

      nil)))

(defn texture [tex-name]
  (str "textures/robot_rampage/" tex-name))

(defn sound [sound-name]
  (str "sounds/robot_rampage/" sound-name))

(defn init []
  (engine/init {:draw-fn   draw*
                :update-fn update*})

  (swap! s/context assoc :state :title-screen)

  (swap! s/context assoc-in [:textures :title-screen] (engine/load-texture (texture "title_screen.png")))
  (swap! s/context assoc-in [:textures :sprite-sheet] (engine/load-texture (texture "sprite_sheet.png")))

  (swap! s/context assoc-in [:player :shot-sound] (sound "shot1.wav"))

  (swap! s/context assoc :difficult-timer 0)

  (swap! s/context assoc-in [:explosion-sounds] (mapv (fn [i] (sound (str "explosion" i ".wav")))
                                                      (range 1 5)))

  (player/init)
  (effects-manager/init)
  (weapon-manager/init)
  (camera/init)
  (tile-map/init)
  (enemy-manager/init))
