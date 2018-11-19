(ns games.core
  (:require [games.engine :as engine]))

(def state (atom {}))

(defn draw* []
  (engine/draw-rectangle
   {:texture (:texture @state)
    :size {:width 60 :height 60}
    :origin {:x 40 :y 40}})
  (engine/draw-text
   {:text "Hello world"
    :font {:size 24}
    :color (engine/color [1.0 1.0 0.0 0.7])
    :origin {:x 200 :y 200}})
  )

(defn update* []
  )

(defn init []
  (engine/init {:draw-fn draw* :update-fn update*})
  (swap! state assoc :texture (engine/load-texture "cubetexture.png"))
  (engine/run))

(init)
