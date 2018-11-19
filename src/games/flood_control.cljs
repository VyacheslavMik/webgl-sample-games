(ns games.flood-control
  (:require [games.engine :as engine]))

(def context (atom {:state :title-screen}))

(defn draw* []
  (let [{:keys [textures state]} @context]
    (case state
      :title-screen
      (engine/draw-rectangle
       {:texture (:title-screen textures)})

      :playing
      (engine/draw-rectangle
       {:texture (:background textures)})

      nil))
  )

;; (engine/draw-rectangle
;;  {:texture (:tile-sheet textures)
;;   :size {:width 40 :height 40}
;;   :tex-coords {:x 1 :y 247 :w 40 :h 40}
;;   :position {:x 1 :y 1}})

;; (engine/draw-rectangle
;;  {:texture (:tile-sheet textures)
;;   :size {:width 40 :height 40}
;;   :tex-coords {:x 1 :y 1 :w 40 :h 40}
;;   :origin {:x 110 :y 40}})

;; (engine/draw-rectangle
;;  {:texture (:tile-sheet textures)
;;   :size {:width 40 :height 40}
;;   :tex-coords {:x 1 :y 247 :w 40 :h 40}
;;   :origin {:x 160 :y 40}})

;; (engine/draw-rectangle
;;  {:texture (:tile-sheet textures)
;;   :size {:width 40 :height 40}
;;   :tex-coords {:x 41 :y 1 :w 40 :h 40}
;;   :color (engine/color [1.0 1.0 0.0 1.0])
;;   :origin {:x 160 :y 40}})

;; (engine/draw-text
;;  {:text "Hello world"
;;   :font {:size 24}
;;   :color (engine/color [1.0 1.0 0.0 0.7])
;;   :origin {:x 200 :y 200}})

(defn update* []
  (let [{:keys [state]} @context]
    (case state
      :title-screen
      (when (engine/key-pressed? :Space)
        (swap! context assoc :state :playing))

      nil))
  )

(defn texture [tex-name]
  (str "textures/flood_control/" tex-name))

(defn run []
  (engine/init {:draw-fn draw* :update-fn update*})

  (swap! context assoc-in [:textures :title-screen] (engine/load-texture (texture "title_screen.png")))
  (swap! context assoc-in [:textures :background]   (engine/load-texture (texture "background.png")))
  (swap! context assoc-in [:textures :tile-sheet]   (engine/load-texture (texture "tile_sheet.png")))

  (engine/run))
