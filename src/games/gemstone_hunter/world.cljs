(ns games.gemstone-hunter.world
  (:require [games.gemstone-hunter.camera :as camera]))

(defn new-container []
  (let [container (js/PIXI.Container.)]
    (set! (.. container -width) 800)
    (set! (.. container -height) 600)
    container))

(defonce background-tile (let [c (new-container)]
                           (.. camera/container (addChild c))
                           c))
(defonce interactive-tile (let [c (new-container)]
                            (.. camera/container (addChild c))
                            c))
(defonce interactive (let [c (new-container)]
                       (.. camera/container (addChild c))
                       c))
(defonce foreground-tile (let [c (new-container)]
                           (.. camera/container (addChild c))
                           c))
(defonce editor (let [c (new-container)]
                  (.. camera/container (addChild c))
                  c))

(.. background-tile  removeChildren)
(.. interactive-tile removeChildren)
(.. foreground-tile  removeChildren)
(.. interactive      removeChildren)
(.. editor           removeChildren) 

(defn add-background-tile [sprite]
  (.. background-tile (addChild sprite)))

(defn add-interactive-tile [sprite]
  (.. interactive-tile (addChild sprite)))

(defn add-interactive [sprite]
  (.. interactive (addChild sprite)))

(defn remove-interactive [sprite]
  (.. interactive (removeChild sprite)))

(defn add-foreground-tile [sprite]
  (.. foreground-tile (addChild sprite)))

(defn add-editor [sprite]
  (.. editor (addChild sprite)))

(defn clear []
  (.. background-tile removeChildren)
  (.. interactive-tile removeChildren)
  (.. foreground-tile removeChildren)
  (.. interactive removeChildren)
  (.. editor removeChildren))
