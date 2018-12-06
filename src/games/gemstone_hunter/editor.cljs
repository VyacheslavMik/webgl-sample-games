(ns games.gemstone-hunter.editor
  (:require [games.gemstone-hunter.tile-map :as tile-map]))

(defn show-tiles []
  (let [tiles (.querySelector js/document "#tiles")]
    (loop [y 0]
      (when (< y 3)
        (loop [x 0]
          (when (< x 10)
            (let [tile      (.createElement js/document "div")
                  tile-text (.createElement js/document "div")
                  index     (+ (* y 10) x)]
              (.. tile-text -classList (add "tile-text"))
              (set! (.-textContent tile-text) (cond
                                                (and (= y 0) (= x 0)) "empty"
                                                (and (= y 0) (= x 1)) "white"
                                                :else                 ""))
              (.appendChild tiles tile-text)

              (.. tile -classList (add "tile"))
              (set! (.. tile -tabIndex) index)
              #_(set! (.. tile -onclick) (fn [ev]
                                         (.. tile -classList (add "tile-selected"))))
              (set! (.. tile -style -background)
                    (str "url(textures/gemstone_hunter/PlatformTiles.png)"
                         " " (- (* x tile-map/tile-width)) "px"
                         " " (- (* y tile-map/tile-height)) "px"))
              (.appendChild tiles tile))
            (recur (inc x))))
        (recur (inc y))))))

(show-tiles)
