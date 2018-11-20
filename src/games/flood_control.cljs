(ns games.flood-control
  (:require [games.engine :as engine]
            [clojure.string :as str]))

(def context (atom {:state :title-screen
                    :board []
                    :falling-pieces {}}))


(def board-width  8)
(def board-height 10)

(def max-playable-piece-index 5)

(def empty-piece {:piece-type :empty
                  :suffixes #{}})

(def texture-offset-x 1)
(def texture-offset-y 1)
(def texture-padding-x 1)
(def texture-padding-y 1)
(def piece-height 40)
(def piece-width 40)
(def piece-types {:left-right   0
                  :top-bottom   1
                  :left-top     2
                  :top-right    3
                  :right-bottom 4
                  :bottom-left  5
                  :empty        6})
(def game-board-display-position-x 70)
(def game-board-display-position-y 89)

(def fall-rate 5)

(defn new-board []
  (swap! context assoc :board
         (mapv (fn [_]
                 (mapv (fn [_])
                       (range board-height)))
               (range board-width))))

(defn clear-board []
  (doseq [x (range board-width)]
    (doseq [y (range board-height)]
      (swap! context assoc-in [:board x y] empty-piece))))

(defn add-falling-piece [x y v-offset]
  (let [piece (get-in @context [:board x y])
        suffix (get-in piece [:suffixes :l])
        piece-type (:piece-type piece)]
    (swap! context assoc-in [:falling-pieces {:x x :y y}]
           {:piece-type piece-type
            :v-offset v-offset
            :suffixes (cond-> #{} suffix (conj suffix))})))

(defn random-piece [x y]
  (let [types [:left-right :top-bottom :left-top :top-right :right-bottom :bottom-left]
        piece-type (get types (rand-int (count types)))
        suffix (when (< (rand) 0.1) :l)]
    (swap! context assoc-in [:board x y] {:piece-type piece-type
                                          :suffixes (cond-> #{} suffix (conj suffix))})))

(defn generate-new-pieces [drop-squares?]
  (doseq [x (range board-width)]
    (doseq [y (range board-height)]
      (when (= (get-in @context [:board x y :piece-type]) :empty)
        (random-piece x y)
        (add-falling-piece x y (* piece-height board-height))))))

(defn draw-background [textures]
  (engine/draw-rectangle
   {:texture (:background textures)}))

(defn get-source-rect [piece]
  {:x (+ texture-offset-x
         (if (get-in piece [:suffixes :w])
           (+ piece-width texture-padding-x)
           0)
         (if (get-in piece [:suffixes :l])
           (* 2 (+ piece-width texture-padding-x))
           0))
   :y (+ texture-offset-y
         (* (piece-types (:piece-type piece))
            (+ piece-height texture-padding-y)))
   :w piece-width
   :h piece-height})

(defn draw-board [{:keys [textures board falling-pieces]}]
  (doseq [x (range board-width)]
    (doseq [y (range board-height)]
      (let [px (+ game-board-display-position-x (* x piece-width))
            py (+ game-board-display-position-y (* y piece-height))]
        (engine/draw-rectangle
         {:texture (:tile-sheet textures)
          :position {:x px :y py}
          :tex-coords (get-source-rect {:piece-type :empty})})
        (cond
          (get falling-pieces {:x x :y y})
          (let [piece (get falling-pieces {:x x :y y})]
            (engine/draw-rectangle
             {:texture (:tile-sheet textures)
              :position {:x px :y (- py (:v-offset piece))}
              :tex-coords (get-source-rect piece)}))

          :else
          (engine/draw-rectangle
           {:texture (:tile-sheet textures)
            :position {:x px :y py}
            :tex-coords (get-source-rect (get-in board [x y]))}))))))

(defn draw* []
  (let [{:keys [textures state board] :as ctx} @context]
    (case state
      :title-screen
      (engine/draw-rectangle
       {:texture (:title-screen textures)})

      :playing
      (do
        (draw-background textures)
        (draw-board      ctx))

      nil))
  )

(defn has-connector? [x y from]
  (str/includes? (name (get-in @context [:board x y :piece-type])) from))

(defn not-filled? [x y]
  (not (get-in @context [:board x y :suffixes :w])))

(defn fill-piece [x y]
  (swap! context update-in [:board x y :suffixes] conj :w))

(defn get-other-end [x y from]
  (-> @context
      (get-in [:board x y :piece-type])
      (name)
      (str/replace from "")
      (str/replace "-" "")))

(defn propagate-water [x y from]
  (when (and (>= y 0) (< y board-height)
             (>= x 0) (< x board-width)
             (has-connector? x y from)
             (not-filled? x y))
    (fill-piece x y)

    (case (get-other-end x y from)
      "left"   (propagate-water (dec x) y "right")
      "right"  (propagate-water (inc x) y "left")
      "top"    (propagate-water x (dec y) "bottom")
      "bottom" (propagate-water x (inc y) "top")

      nil)))

(defn get-water-chain [y]
  (propagate-water 0 y "left")
  )

(defn reset-water []
  (doseq [x (range board-width)]
    (doseq [y (range board-height)]
      (swap! context update-in [:board x y :suffixes] disj :w))))

(defn are-pieces-animating? []
  (not (empty? (:falling-pieces @context))))

(defn update-falling-piece [k delta]
  (swap! context update-in [:falling-pieces k :v-offset] (fn [v] (max 0 (- v fall-rate)))))

(defn update-falling-pieces [delta]
  (doseq [k (keys (:falling-pieces @context))]
    (update-falling-piece k delta)
    (when (= (get-in @context [:falling-pieces k :v-offset]) 0)
      (swap! context update :falling-pieces dissoc k))))

(defn update-animated-pieces [delta]
  (update-falling-pieces delta))

(defn update* [delta]
  (let [{:keys [state]} @context]
    (case state
      :title-screen
      (when (engine/key-pressed? :Space)
        (new-board)
        (clear-board)
        (generate-new-pieces false)
        (swap! context assoc :state :playing))

      :playing
      (do
        (if (are-pieces-animating?)
          (update-animated-pieces delta)
          (do
            (reset-water)
            (doseq [y (range board-height)]
              (get-water-chain y))
            )))

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
