(ns games.robot-rampage.tile-map
  (:require [games.engine :as engine]
            [games.robot-rampage.utils :as u]
            [games.robot-rampage.storage :as s]
            [games.robot-rampage.camera :as camera]))

(defonce container (let [container (js/PIXI.Container.)]
                     (set! (.. container -width) 800)
                     (set! (.. container -height) 600)
                     container))

(def tile-width 32)
(def tile-height 32)
(def ^:export map-width 50)
(def ^:export map-height 50)

(def floor-tile-start 0)
(def floor-tile-end 3)
(def ^:export wall-tile-start 4)
(def wall-tile-end 7)

(defn tile-rect [x y] {:x x :y y :w tile-width :h tile-height})

(def tiles [(tile-rect 0 0)
            (tile-rect 32 0)
            (tile-rect 64 0)
            (tile-rect 96 0)
            (tile-rect 0 32)
            (tile-rect 32 32)
            (tile-rect 64 32)
            (tile-rect 96 32)])

(defn get-square-by-pixel-x [pixel-x] (Math/floor (/ pixel-x tile-width)))
(defn get-square-by-pixel-y [pixel-y] (Math/floor (/ pixel-y tile-height)))

(defn get-square-at-pixel [pixel-location]
  {:x (get-square-by-pixel-x (:x pixel-location))
   :y (get-square-by-pixel-y (:y pixel-location))})

(defn get-square-center
  ([square]
   (get-square-center (:x square) (:y square)))

  ([square-x square-y]
   {:x (+ (* square-x tile-width) (/ tile-width 2))
    :y (+ (* square-y tile-height) (/ tile-height 2))}))

(defn square-world-rectangle
  ([square]
   (square-world-rectangle (:x square) (:y square)))
  
  ([x y]
   {:x (* x tile-width)
    :y (* y tile-height)
    :width tile-width
    :height tile-height}))

(defn square-screen-rectangle
  ([square]
   (square-screen-rectangle (:x square) (:y square)))

  ([x y]
   (camera/transform-rectangle (square-world-rectangle x y))))

(defn get-tile-at-square [tile-x tile-y]
  (if (and (>= tile-x 0) (< tile-x map-width)
           (>= tile-y 0) (< tile-y map-height))
    (aget (:map-squares @s/context) tile-x tile-y)
    -1))

(defn set-tile-at-square [tile-x tile-y tile]
  (when (and (>= tile-x 0) (< tile-x map-width)
             (>= tile-y 0) (< tile-y map-height))
    (swap! s/context update :map-squares aset tile-x tile-y tile)))

(defn get-tile-at-pixel
  ([pixel-location]
   (get-tile-at-pixel (:x pixel-location) (:y pixel-location)))
  
  ([pixel-x pixel-y]
   (get-tile-at-square (get-square-by-pixel-x pixel-x)
                       (get-square-by-pixel-y pixel-y))))

(defn wall-tile?
  ([square]
   (wall-tile? (:x square) (:y square)))

  ([tile-x tile-y]
   (let [tile-index (get-tile-at-square tile-x tile-y)]
     (if (= tile-index -1)
       false
       (>= tile-index wall-tile-start)))))

(defn wall-tile-by-pixel? [pixel-location]
  (wall-tile? (get-square-by-pixel-x (:x pixel-location))
              (get-square-by-pixel-y (:y pixel-location))))

(def sprites (array))

(defn get-sprite [x y]
  (when-let [a (aget sprites x)]
    (aget a y)))

(defn set-sprite [x y sprite]
  (when-not (aget sprites x)
    (aset sprites x (array)))
  (aset sprites x y sprite))

(defn make-sprite [x y tile]
  (when-let [sprite (get-sprite x y)]
    (.. sprite destroy))
  (let [sprite (u/pixi-sprite (get tiles tile) container)
        pos (square-world-rectangle x y)]
    (.. sprite -position (set (:x pos) (:y pos)))
    (set-sprite x y sprite))
  tile)

(defn generate-random-map []
  (let [wall-change-per-square 10
        floor-tile (u/random-int floor-tile-start (inc floor-tile-end))
        wall-tile (u/random-int wall-tile-start (inc wall-tile-end))]
    (swap! s/context assoc :map-squares 
           (clj->js
            (mapv (fn [x]
                    (mapv (fn [y]
                            (make-sprite
                             x y
                             (cond
                               (or (= x 0) (= y 0) (= x (dec map-width)) (= y (dec map-height)))
                               wall-tile

                               (or (= x 1) (= y 1) (= x (- map-width 2)) (= y (- map-height 2)))
                               floor-tile

                               (<= (rand-int 100) wall-change-per-square)
                               wall-tile

                               :else
                               floor-tile)))
                          (range map-height)))
                  (range map-width))))))
