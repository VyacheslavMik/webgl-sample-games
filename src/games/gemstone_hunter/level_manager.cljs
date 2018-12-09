(ns games.gemstone-hunter.level-manager
  (:require [goog.string :as gstring]
            [goog.string.format]
            [games.gemstone-hunter.player :as player]
            [games.gemstone-hunter.game-object :as game-object]
            [games.gemstone-hunter.tile-map :as tile-map]))

(def context (atom {}))

(defn init []
  (swap! context assoc :player (player/new-player)))

(defn load-level [level-number]
  (-> (js/fetch (gstring/format "maps/gemstone_hunter/MAP%03d" level-number))
      (.then (fn [response]
               (.text response)))
      (.then (fn [s]
               (tile-map/load-map s)
               (dotimes [x tile-map/map-width]
                 (dotimes [y tile-map/map-height]
                   (let [code (tile-map/cell-code-value x y)]
                     (when (= code "START")
                       (swap! context assoc-in [:player :world-location] {:x (* x tile-map/tile-width)
                                                                          :y (* y tile-map/tile-height)}))
                     )))))))

(defn draw* []
  (game-object/draw* (:player @context)))

(defn update* [elapsed]
  (let [player (player/update* (:player @context) elapsed)]
    (swap! context assoc :player player))
  )
