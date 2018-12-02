(ns games.robot-rampage.path-finder
  (:require [games.robot-rampage.storage :as s]))

(defn find-path [start-tile end-tile]
  (let [tiles (:map-squares @s/context)
        start-tile (clj->js start-tile)
        end-tile (clj->js end-tile)]
    (js->clj (js/findPath start-tile end-tile tiles))))
