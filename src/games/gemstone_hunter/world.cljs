(ns games.gemstone-hunter.world
  (:require [games.gemstone-hunter.camera :as camera]))

(defn new-container []
  (let [container (js/PIXI.Container.)]
    (set! (.. container -width) 800)
    (set! (.. container -height) 600)
    container))

(def background-tile (new-container))
(def interactive-tile (new-container))
(def foreground-tile (new-container))
(def interactive (new-container))
(def editor (new-container))

(.. camera/container (addChild background-tile))
(.. camera/container (addChild interactive-tile))
(.. camera/container (addChild interactive))
(.. camera/container (addChild foreground-tile))
(.. camera/container (addChild editor))

(defn add-background-tile [sprite]
  (.. background-tile (addChild sprite)))

(defn add-interactive-tile [sprite]
  (.. interactive-tile (addChild sprite)))

(defn add-interactive [sprite]
  (.. interactive (addChild sprite)))

(defn add-foreground-tile [sprite]
  (.. foreground-tile (addChild sprite)))

(defn add-editor [sprite]
  (.. editor (addChild sprite)))
