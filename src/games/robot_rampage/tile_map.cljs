(ns games.robot-rampage.tile-map
  (:require [games.engine :as engine]
            [games.robot-rampage.utils :as u]
            [games.robot-rampage.storage :as s]
            [games.robot-rampage.camera :as camera]))

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

(defn generate-random-map []
  (let [wall-change-per-square 10
        floor-tile (u/random-int floor-tile-start (inc floor-tile-end))
        wall-tile (u/random-int wall-tile-start (inc wall-tile-end))]
    (clj->js
     (mapv (fn [x]
             (mapv (fn [y]
                     (cond
                       (or (= x 0) (= y 0) (= x (dec map-width)) (= y (dec map-height)))
                       wall-tile

                       (or (= x 1) (= y 1) (= x (- map-width 2)) (= y (- map-height 2)))
                       floor-tile

                       (<= (rand-int 100) wall-change-per-square)
                       wall-tile

                       :else
                       floor-tile))
                   (range map-height)))
           (range map-width)))))

(defn init []
  (swap! s/context assoc :map-squares (generate-random-map)))

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

(defn draw* []
  (let [texture (get-in @s/context [:textures :sprite-sheet])
        {:keys [x y]} (camera/position)

        start-x (get-square-by-pixel-x x)
        end-x (get-square-by-pixel-x (+ x (camera/view-port-width)))

        start-y (get-square-by-pixel-y y)
        end-y (get-square-by-pixel-y (+ y (camera/view-port-height)))]
    (doseq [x (range start-x (inc end-x))]
      (doseq [y (range start-y (inc end-y))]
        (when (and (>= x 0) (< x map-width)
                   (>= y 0) (< y map-height))
          (engine/draw-rectangle
           {:texture texture
            :position (square-screen-rectangle x y)
            :tex-coords (get tiles (get-tile-at-square x y))}))))))
