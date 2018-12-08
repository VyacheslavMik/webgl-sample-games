(ns games.gemstone-hunter.editor
  (:require [games.engine :as engine]
            [goog.string :as gstring]
            [goog.string.format]
            [games.gemstone-hunter.camera :as camera]
            [games.gemstone-hunter.utils :as u]
            [games.gemstone-hunter.tile-map :as tile-map]))

(defonce context (atom {:layer :background
                        :mode :toggle-passable
                        :code "CUSTOM"
                        :tile-index 0
                        :map-number "000"
                        :last-mouse-state nil
                        :code-value nil}))

(defn set-disable-code-inputs []
  (let [code-value (.querySelector js/document "#code-value")
        codes      (.querySelector js/document "#codes")]
    (if (and (= (:code @context) "CUSTOM")
             (= (:mode @context) :code))
      (set! (.. code-value -disabled) false)
      (set! (.. code-value -disabled) true))
    (if (= (:mode @context) :toggle-passable)
      (set! (.. codes -disabled) true)
      (set! (.. codes -disabled) false))))

(defn set-code-input-value []
  (let [code-value (.querySelector js/document "#code-value")]
    (set! (.. code-value -value) (:code-value @context))))

(defn ^:export change-layer [ev]
  (swap! context assoc :layer (keyword (.. ev -target -value))))

(defn ^:export change-mode [ev]
  (let [mode (keyword (.. ev -target -value))]
    (swap! context assoc :mode mode)
    (set-disable-code-inputs)))

(defn ^:export change-code [ev]
  (let [code (.. ev -target -value)]
    (swap! context assoc :code code)
    (if (#{"CLEAR" "CUSTOM"} code)
      (swap! context assoc :code-value nil)
      (swap! context assoc :code-value code))
    (set-code-input-value)
    (set-disable-code-inputs)))

(defn ^:export custom-code [ev]
  (swap! context assoc :code-value (.. ev -target -value)))

(defn ^:export change-map-number [ev]
  (println (.. ev -target -value))
  (swap! context assoc :map-number (.. ev -target -value)))

(defn ^:export save-map []
  (let [element (.createElement js/document "a")]
    (.setAttribute element "href" (str "data:application/octet-stream;charset=utf-8,"
                                       (js/encodeURIComponent
                                        (tile-map/save-map))))
    (.setAttribute element "download" (str "MAP" (:map-number @context)))
    (set! (.. element -style -display) "none")
    (.appendChild (.. js/document -body) element)
    (.click element)
    (.removeChild (.. js/document -body) element)))

(defn ^:export load-map [ev]
  (let [fr (js/FileReader.)]
    (aset fr "onload" (fn [e]
                        (tile-map/load-map (.. e -target -result))))
    (.readAsText fr (aget (.. ev -target -files) 0)))
  (set! (.. ev -target -value) ""))

(defn fill-tiles []
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
              (set! (.. tile -onclick) (fn [ev]
                                         (swap! context assoc :tile-index index)))
              (set! (.. tile -style -background)
                    (str "url(textures/gemstone_hunter/PlatformTiles.png)"
                         " " (- (* x tile-map/tile-width)) "px"
                         " " (- (* y tile-map/tile-height)) "px"))
              (.appendChild tiles tile))
            (recur (inc x))))
        (recur (inc y))))))

(defn fill-map-numbers []
  (let [map-numbers (.querySelector js/document "#map-numbers")]
    (dotimes [i 100]
      (let [map-number (.createElement js/document "option")]
        (when (= i 0)
          (.setAttribute map-number "checked" true))
        (set! (.-textContent map-number) (gstring/format "%03d" i))
        (set! (.-value map-number) (gstring/format "%03d" i))
        (.appendChild map-numbers map-number)))))

(defn draw* []
  (tile-map/draw*))

(defn update* [delta]
  (let [ms (engine/get-mouse-state)]
    (when (and (> (:x ms) 0) (< (:x ms) (camera/view-port-width))
               (> (:y ms) 0) (< (:y ms) (camera/view-port-height)))
      (let [x (:x ms) y (:y ms)]
        (when (u/rectangle-contains (camera/world-rectangle) x y)
          (when (= (:button ms) :left)
            (tile-map/set-tile-at-cell (tile-map/get-cell-by-pixel-x x)
                                       (tile-map/get-cell-by-pixel-y y)
                                       (:layer @context)
                                       (:tile-index @context)))
          (when (and (not= (-> @context :last-mouse-state :button) :right)
                     (= (:button ms) :right))
            (if (= (:mode @context) :toggle-passable)
              (tile-map/toggle-passable (tile-map/get-cell-by-pixel-x x)
                                        (tile-map/get-cell-by-pixel-y y))
              (tile-map/set-code (tile-map/get-cell-by-pixel-x x)
                                 (tile-map/get-cell-by-pixel-y y)
                                 (:code-value @context)))))))
    (swap! context assoc :last-mouse-state ms)))

(when-not (:initialized? @context)
  (swap! context assoc :initialized? true)
  (fill-tiles)
  (fill-map-numbers)
  (set-disable-code-inputs)
  (engine/init {:draw-fn draw*
                :update-fn update*})
  (tile-map/initialize
   (engine/load-texture "textures/gemstone_hunter/PlatformTiles.png"))
  (tile-map/set-editor-mode true)
  (camera/initialize {:view-port-width 800
                      :view-port-height 600
                      :world-rectangle {:x 0 :y 0
                                        :width (* tile-map/tile-width tile-map/map-width)
                                        :height (* tile-map/tile-height tile-map/map-height)}})
  (engine/run))
