(ns games.core
  (:require [games.engine :as engine]))

(def state (atom {}))

(defn draw* []
  (let [{:keys [textures]} @state]
    (engine/draw-rectangle
     {:texture (:cube textures)
      :size {:width 60 :height 60}
      :origin {:x 40 :y 40}})

    (engine/draw-rectangle
     {:texture (:tile-sheet textures)
      :size {:width 40 :height 40}
      :tex-coords {:x 1 :y 247 :w 40 :h 40}
      :origin {:x 110 :y 40}})

    (engine/draw-rectangle
     {:texture (:tile-sheet textures)
      :size {:width 40 :height 40}
      :tex-coords {:x 1 :y 1 :w 40 :h 40}
      :origin {:x 110 :y 40}})

    (engine/draw-rectangle
     {:texture (:tile-sheet textures)
      :size {:width 40 :height 40}
      :tex-coords {:x 1 :y 247 :w 40 :h 40}
      :origin {:x 160 :y 40}})

    (engine/draw-rectangle
     {:texture (:tile-sheet textures)
      :size {:width 40 :height 40}
      :tex-coords {:x 41 :y 1 :w 40 :h 40}
      :color (engine/color [1.0 1.0 0.0 1.0])
      :origin {:x 160 :y 40}})

    (engine/draw-text
     {:text "Hello world"
      :font {:size 24}
      :color (engine/color [1.0 1.0 0.0 0.7])
      :origin {:x 200 :y 200}}))
  )

(defn update* []
  )

(defn init []
  (engine/init {:draw-fn draw* :update-fn update*})
  (swap! state assoc-in [:textures :cube]       (engine/load-texture "cubetexture.png"))
  (swap! state assoc-in [:textures :tile-sheet] (engine/load-texture "textures/flood_control/tile_sheet.png"))
  (engine/run))

(init)
