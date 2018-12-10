(ns games.gemstone-hunter.gemstone
  (:require [games.engine :as engine]
            [games.gemstone-hunter.tile-map :as tile-map]
            [games.gemstone-hunter.animation-strip :as anim]
            [games.gemstone-hunter.game-object :as game-object]))

(defn load-texture []
  (engine/load-texture "textures/gemstone_hunter/Gem.png"))

(defn new-gemstone [cell-x cell-y]
  (let [ob (game-object/new-game-object)
        animations {"idle" (-> (anim/new-animation-strip (load-texture) 48 "idle")
                               (assoc :loop-animation? true
                                      :frame-delay 0.15))}]
    (-> ob
        (assoc :animations animations
               :world-location {:x (* cell-x tile-map/tile-width)
                                :y (* cell-y tile-map/tile-height)}
               :frame-width  tile-map/tile-width
               :frame-height tile-map/tile-height
               :collision-rectangle {:x 9 :y 24 :width 30 :height 24}
               :draw-depth 6
               :enabled? true)
        (game-object/play-animation "idle"))))
