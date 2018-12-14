(ns games.gemstone-hunter.tile-map
  (:require [games.engine :as engine]
            [cljs.reader]
            [games.gemstone-hunter.utils :as u]
            [games.gemstone-hunter.world :as world]
            [games.gemstone-hunter.camera :as camera]
            [clojure.string :as str]))

(def tile-width 48)
(def tile-height 48)
(def map-width 160)
(def map-height 12)
(def tiles-per-row 10)

(def sky-tile 2)

(def context (atom {:editor-mode? false
                    :map-cells (array)}))

(defn load-tiles []
  (let [bt (js/PIXI.BaseTexture.fromImage "textures/gemstone_hunter/PlatformTiles.png")]
    (reduce (fn [acc i]
              (assoc acc i
                     (js/PIXI.Texture. bt
                                       (js/PIXI.Rectangle.
                                        (* (mod i tiles-per-row) tile-width)
                                        (* (Math/floor (/ i tiles-per-row)) tile-height)
                                        tile-width
                                        tile-height)
                                       (js/PIXI.Rectangle.
                                        (* (mod i tiles-per-row) tile-width)
                                        (* (Math/floor (/ i tiles-per-row)) tile-height)
                                        tile-width
                                        tile-height))))
            {} (range 30))))

(def tile-textures (load-tiles))

(defn tile-rect [x y] {:x x :y y :w tile-width :h tile-height})

(defn update-tile [x y tile]
  (let [draw (:draw tile)
        x (* x tile-width)
        y (* y tile-height)]
    (if (.. draw -sprite)
      (do
        (set! (.. draw -sprite -background -texture) (get tile-textures (:background tile)))
        (set! (.. draw -sprite -interactive -texture) (get tile-textures (:interactive tile)))
        (set! (.. draw -sprite -foreground -texture) (get tile-textures (:foreground tile))))
      (let [background (js/PIXI.Sprite. (get tile-textures (:background tile)))
            interactive (js/PIXI.Sprite. (get tile-textures (:interactive tile)))
            foreground (js/PIXI.Sprite. (get tile-textures (:foreground tile)))]
        (.. background -position (set x y))
        (.. interactive -position (set x y))
        (.. foreground -position (set x y))
        (world/add-background-tile background)
        (world/add-interactive-tile interactive)
        (world/add-foreground-tile foreground)
        (set! (.. draw -sprite) #js {:background background
                                     :interactive interactive
                                     :foreground foreground})))
    (when (:editor-mode? @context)
      (if (.. draw -passable)
        (set! (.. draw -passable -visible) (not (boolean (:passable? tile))))
        (let [sprite (js/PIXI.Sprite. (get tile-textures 1))]
          (set! (.. sprite -tint) 0xff0000)
          (set! (.. sprite -visible) (not (boolean (:passable? tile))))
          (set! (.. sprite -alpha) 0.5)
          (.. sprite -position (set x y))
          (world/add-editor sprite)
          (set! (.. draw -passable) sprite)))
      (if (str/blank? (:code tile))
        (when (.. draw -text)
          (.. draw -text destroy)
          (set! (.. draw -text) nil))
        (if (.. draw -text)
          (set! (.. draw -text -text) (:code tile))
          (let [text (js/PIXI.Text. (:code tile) #js {:fontFamily "Arial"
                                                      :fontSize 10
                                                      :fill "white"})]
            (.. text -position (set x y))
            (world/add-editor text)
            (set! (.. draw -text) text)))))
    tile))

(defn clear-map []
  (let [map-cells (:map-cells @context)]
    (dotimes [x map-width]
      (dotimes [y map-height]
        (let [tile {:background sky-tile
                    :interactive 0
                    :foreground 0
                    :code nil
                    :draw #js {}
                    :passable? true}]
          (aset map-cells x y tile)
          (update-tile x y tile))))))

(defn initialize [editor-mode?]
  (swap! context assoc :editor-mode? editor-mode?)
  (swap! context assoc :map-cells
         (clj->js
          (mapv (fn [_]
                  (mapv (fn [_] nil)
                        (range map-height)))
                (range map-width))))
  (clear-map))

(defn set-editor-mode [b]
  (swap! context assoc :editor-mode? b))

(defn tile-source-rectangle [tile-index]
  {:x (* (mod tile-index tiles-per-row) tile-width)
     :y (* (Math/floor (/ tile-index tiles-per-row)) tile-height)
     :w tile-width
     :h tile-height})

(defn get-cell-by-pixel-x [pixel-x] (Math/floor (/ pixel-x tile-width)))
(defn get-cell-by-pixel-y [pixel-y] (Math/floor (/ pixel-y tile-height)))

(defn get-cell-by-pixel [pixel-location]
  {:x (get-cell-by-pixel-x (:x pixel-location))
   :y (get-cell-by-pixel-y (:y pixel-location))})

(defn get-cell-center
  ([cell]
   (get-cell-center (:x cell) (:y cell)))

  ([cell-x cell-y]
   {:x (+ (* cell-x tile-width) (/ tile-width 2))
    :y (+ (* cell-y tile-height) (/ tile-height 2))}))

(defn cell-world-rectangle
  ([cell]
   (cell-world-rectangle (:x cell) (:y cell)))
  
  ([x y]
   {:x (* x tile-width)
    :y (* y tile-height)
    :width tile-width
    :height tile-height}))

(defn cell-screen-rectangle
  ([cell]
   (cell-screen-rectangle (:x cell) (:y cell)))

  ([x y]
   (camera/world-to-screen-r (cell-world-rectangle x y))))

(defn get-map-square-at-cell [tile-x tile-y]
  (when (and (>= tile-x 0) (< tile-x map-width)
             (>= tile-y 0) (< tile-y map-height))
    (aget (:map-cells @context) tile-x tile-y)))

(defn set-tile-at-cell [tile-x tile-y layer tile-index]
  (when (and (>= tile-x 0) (< tile-x map-width)
             (>= tile-y 0) (< tile-y map-height))
    (let [tile (-> (aget (:map-cells @context) tile-x tile-y)
                   (assoc layer tile-index))]
      (aset (:map-cells @context) tile-x tile-y tile)
      (update-tile tile-x tile-y tile))))

(defn cell-passable?
  ([cell]
   (cell-passable? (:x cell) (:y cell)))

  ([cell-x cell-y]
   (when-let [square (get-map-square-at-cell cell-x cell-y)]
     (:passable? square))))

(defn cell-passable-by-pixel? [pixel-location]
  (cell-passable? (get-cell-by-pixel-x (:x pixel-location))
                  (get-cell-by-pixel-y (:y pixel-location))))

(defn cell-code-value
  ([cell]
   (cell-code-value (:x cell) (:y cell)))
  
  ([cell-x cell-y]
   (when-let [square (get-map-square-at-cell cell-x cell-y)]
     (:code square))))

(defn get-map-square-at-pixel
  ([pixel-location]
   (get-map-square-at-pixel (:x pixel-location) (:y pixel-location)))

  ([pixel-x pixel-y]
   (get-map-square-at-cell (get-cell-by-pixel-x pixel-x)
                           (get-cell-by-pixel-y pixel-y))))

(defn draw-edit-mode-items [map-cells texture x y]
  (when-not (or (< x 0) (>= x map-width)
                (< y 0) (>= y map-height))
    (when-not (cell-passable? x y)
      (engine/draw-rectangle {:texture texture
                              :position (cell-screen-rectangle x y)
                              :tex-coords (tile-source-rectangle 1)
                              :color (engine/rgb-color [255 0 0 127])}))
    (when-let [code (:code (aget map-cells x y))]
      (let [screen-rect (cell-screen-rectangle x y)]
        (engine/draw-text {:text code
                           :scale 0.7
                           :align :start
                           :position {:x (:x screen-rect)
                                      :y (+ (:y screen-rect) 5)}})))))

(defn draw* []
  (let [{:keys [texture map-cells editor-mode?]} @context
        {{x :x y :y} :position
         view-port-width :view-port-width
         view-port-height :view-port-height} @camera/context

        start-x (get-cell-by-pixel-x x)
        end-x   (get-cell-by-pixel-x (+ x view-port-width))

        start-y (get-cell-by-pixel-y y)
        end-y   (get-cell-by-pixel-y (+ y view-port-height))]
    (loop [x start-x]
      (when (<= x end-x)
        (loop [y start-y]
          (when (<= y end-y)
            (when (and (>= x 0) (< x map-width)
                       (>= y 0) (< y map-height))
              (engine/draw-rectangle {:texture texture
                                      :position (cell-screen-rectangle x y)
                                      :depth 0
                                      :tex-coords (tile-source-rectangle
                                                   (:background
                                                    (aget map-cells x y)))})
              (engine/draw-rectangle {:texture texture
                                      :depth 5
                                      :position (cell-screen-rectangle x y)
                                      :tex-coords (tile-source-rectangle
                                                   (:interactive
                                                    (aget map-cells x y)))})
              (engine/draw-rectangle {:texture texture
                                      :depth 9
                                      :position (cell-screen-rectangle x y)
                                      :tex-coords (tile-source-rectangle
                                                   (:foreground
                                                    (aget map-cells x y)))}))
            (when editor-mode?
              (draw-edit-mode-items map-cells texture x y))
            (recur (inc y))))
        (recur (inc x))))))

(defn save-map []
  (str
   (mapv (fn [a]
           (mapv (fn [v] (dissoc v :draw)) a))
         (:map-cells @context))))

(defn load-map [cells]
  (let [arr (to-array
             (map (fn [ys]
                    (to-array
                     (map
                      (fn [tile] (assoc tile :draw #js{}))
                      ys)))
                  (cljs.reader/read-string cells)))]
    (dotimes [x map-width]
      (dotimes [y map-height]
        (let [tile (aget arr x y)]
          (update-tile x y tile))))
    (swap! context assoc :map-cells arr)))

(defn toggle-passable [x y]
  (let [map-cells (:map-cells @context)]
    (aset map-cells x y
          (update (aget map-cells x y)
                  :passable? not))
    (update-tile x y (aget map-cells x y))))

(defn set-code [x y code]
  (let [map-cells (:map-cells @context)]
    (aset map-cells x y
          (assoc (aget map-cells x y)
                 :code code))
    (update-tile x y (aget map-cells x y))))
