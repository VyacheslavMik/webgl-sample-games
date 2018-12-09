(ns games.gemstone-hunter.core
  (:require [games.engine :as engine]
            [games.gemstone-hunter.level-manager :as level-manager]
            [games.gemstone-hunter.tile-map :as tile-map]
            [games.gemstone-hunter.player :as player]
            [games.gemstone-hunter.game-object :as game-object]
            [games.gemstone-hunter.camera :as camera]))

(def context (atom {}))

(defn draw* []
  (let [{:keys [state texture]} @context]
    (when (= state :title-screen)
      (engine/draw-rectangle
       {:texture texture
        :size {:width 800 :height 600}}))

    (when (#{:playing :player-dead :game-over} state)
      (tile-map/draw*)
      (level-manager/draw*))
    ))

(defn start-new-game []
  (level-manager/load-level 0))

(defn update* [delta]
  (let [{:keys [state]} @context
        elapsed (* delta 0.001)]
    (case state
      :title-screen
      (do
        (when (or (engine/key-pressed? :Space) (engine/get-touch-state))
          (start-new-game)
          (swap! context assoc :state :playing)))

      :playing
      (do
        (level-manager/update* elapsed))

      :game-over
      (do)

      nil)))

(defn texture [tex-name]
  (str "textures/gemstone_hunter/" tex-name))

(defn init []
  (engine/init {:draw-fn   draw*
                :update-fn update*})

  (swap! context assoc :state :title-screen)
  (swap! context assoc :texture (engine/load-texture (texture "TitleScreen.png")))
  (tile-map/initialize (engine/load-texture (texture "PlatformTiles.png")))

  (camera/initialize {:view-port-width 800
                      :view-port-height 600
                      :world-rectangle {:x 0 :y 0
                                        :width (* tile-map/tile-width tile-map/map-width)
                                        :height (* tile-map/tile-height tile-map/map-height)}})
  (level-manager/init)
)
