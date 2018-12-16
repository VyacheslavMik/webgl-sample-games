(ns games.gemstone-hunter.core
  (:require [games.engine :as engine]
            [games.gemstone-hunter.level-manager :as level-manager]
            [games.gemstone-hunter.tile-map :as tile-map]
            [games.gemstone-hunter.player :as player]
            [games.gemstone-hunter.game-object :as game-object]
            [games.game :as game]
            [games.pixi :as pixi]
            [games.controls :as controls]
            [games.gemstone-hunter.camera :as camera]))

(defonce context (atom {}))

(defonce background (let [sprite (js/PIXI.Sprite.
                                  (js/PIXI.Texture.fromImage
                                   "textures/gemstone_hunter/TitleScreen.png"))]
                      (set! (.. sprite -width) 800)
                      (set! (.. sprite -height) 600)
                      sprite))
(defonce root (let [container (js/PIXI.Container.)]
                (set! (.. container -width) 800)
                (set! (.. container -height) 600)
                container))

(defonce score-text (let [text (js/PIXI.Text. (str "Score: " 0)
                                              #js{:fontFamily "Arial"
                                                  :fontSize 16
                                                  :fill "white"})]
                      (.. text -position (set 20 580))
                      (set! (.. text -visible) false)
                      text))

(defonce lives-text (let [text (js/PIXI.Text. (str "Lives Remaining " 0)
                                              #js{:fontFamily "Arial"
                                                  :fontSize 16
                                                  :fill "white"})]
                      (.. text -position (set 600 580))
                      (set! (.. text -visible) false)
                      text))

(defonce game-over-text (let [text (js/PIXI.Text. "G A M E  O V E R !"
                                                  #js{:fontFamily "Arial"
                                                      :fontSize 48
                                                      :fill "white"})]
                          (.. text -position (set 400 300))
                          (.. text -anchor (set 0.5))
                          (set! (.. text -visible) false)
                          text))

(defn start-new-game []
  (level-manager/init)
  (level-manager/load-level 0))

(defn update* [delta]
  (let [{:keys [state]} @context
        elapsed (* delta 0.001)]
    (case state
      :title-screen
      (do
        (when (or (controls/key-pressed? :Space) (controls/get-touch-state))
          (start-new-game)
          (swap! context assoc :state :playing)
          (set! (.. background -visible) false)
          (set! (.. camera/container -visible) true)
          (set! (.. score-text -visible) true)
          (set! (.. lives-text -visible) true)))

      :playing
      (do
        (level-manager/update* elapsed)
        (let [player (:player @level-manager/context)]
          (let [s (str "Score: " (:score player))]
            (when-not (= s (.. score-text -text))
              (set! (.. score-text -text) s)))
          (let [s (str "Lives Remaining: " (:lives-remaining player))]
            (when-not (= s (.. lives-text -text))
              (set! (.. lives-text -text) s)))
          (when (:dead? player)
            (if (> (:lives-remaining player) 0)
              (do
                (swap! context assoc :state :player-dead)
                (swap! context assoc :death-timer 0))
              (do
                (set! (.. game-over-text -visible) true)
                (swap! context assoc :state :game-over)
                (swap! context assoc :death-timer 0))))))

      :player-dead
      (do
        (level-manager/update* elapsed)
        (swap! context update :death-timer + elapsed)
        (when (> (:death-timer @context) 5)
          (swap! context assoc :state :playing)
          (level-manager/revive-player)
          (level-manager/reload-level)))

      :game-over
      (do
        (swap! context update :death-timer + elapsed)
        (when (> (:death-timer @context) 5)
          (set! (.. game-over-text -visible) false)
          (set! (.. score-text -visible) false)
          (set! (.. lives-text -visible) false)
          (set! (.. camera/container -visible) false)
          (set! (.. background -visible) true)
          (swap! context assoc :state :title-screen)))

      nil)))

(defn init []
  (swap! context assoc :state :title-screen)
  (swap! context assoc :death-timer 0)
  (tile-map/initialize false)

  (camera/initialize {:view-port-width 800
                      :view-port-height 600
                      :world-rectangle {:x 0 :y 0
                                        :width (* tile-map/tile-width tile-map/map-width)
                                        :height (* tile-map/tile-height tile-map/map-height)}})
  (set! (.. camera/container -visible) false)
  (when-not (:initialized? @context)
    (.. root (addChild camera/container))
    (.. root (addChild background))
    (.. root (addChild score-text))
    (.. root (addChild lives-text))
    (.. root (addChild game-over-text))
    (game/run (pixi/init
               ["textures/gemstone_hunter/PlatformTiles.png"
                "textures/gemstone_hunter/Sprites/Player/Idle.png"
                "textures/gemstone_hunter/Sprites/Player/Run.png"
                "textures/gemstone_hunter/Sprites/Player/Jump.png"
                "textures/gemstone_hunter/Sprites/Player/Die.png"
                "textures/gemstone_hunter/Gem.png"
                
                "textures/gemstone_hunter/Sprites/MonsterA/Idle.png"
                "textures/gemstone_hunter/Sprites/MonsterA/Run.png"
                "textures/gemstone_hunter/Sprites/MonsterA/Die.png"
                
                "textures/gemstone_hunter/Sprites/MonsterB/Idle.png"
                "textures/gemstone_hunter/Sprites/MonsterB/Run.png"
                "textures/gemstone_hunter/Sprites/MonsterB/Die.png"
                
                "textures/gemstone_hunter/Sprites/MonsterC/Idle.png"
                "textures/gemstone_hunter/Sprites/MonsterC/Run.png"
                "textures/gemstone_hunter/Sprites/MonsterC/Die.png"
                
                "textures/gemstone_hunter/Sprites/MonsterD/Idle.png"
                "textures/gemstone_hunter/Sprites/MonsterD/Run.png"
                "textures/gemstone_hunter/Sprites/MonsterD/Die.png"]
               #()) update* root)
    (swap! context assoc :initialized? true)))
