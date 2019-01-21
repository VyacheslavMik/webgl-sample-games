(ns games.flood-control.core
  (:require [games.pixi :as pixi]
            [games.game :as game]
            [games.controls :as controls]
            [clojure.string :as str]))

(def context (atom {:state :title-screen
                    :time-since-last-input 0
                    :board []
                    :flood-count 0
                    :time-since-last-flood-increase 0
                    :flood-increase-amount 0
                    :current-level 0
                    :lines-completed-this-level 0
                    :score-zooms []
                    :falling-pieces {}
                    :rotating-pieces {}
                    :fading-pieces {}}))

(defonce root (let [container (js/PIXI.Container.)]
                (set! (.. container -width) 800)
                (set! (.. container -height) 600)
                container))

(defonce board (let [container (js/PIXI.Container.)]
                 (set! (.. container -width) 800)
                 (set! (.. container -height) 600)
                 container))

(defonce title-screen (let [sprite (js/PIXI.Sprite.
                                    (js/PIXI.Texture.fromImage
                                     "textures/flood_control/title_screen.png"))]
                        (set! (.. sprite -width) 800)
                        (set! (.. sprite -height) 600)
                        sprite))

(defonce background (let [sprite (js/PIXI.Sprite.
                                  (js/PIXI.Texture.fromImage
                                   "textures/flood_control/background.png"))]
                      (set! (.. sprite -width) 800)
                      (set! (.. sprite -height) 600)
                      sprite))

(defonce game-over-text (let [text (js/PIXI.Text. "G A M E  O V E R !"
                                                  #js{:fontFamily "Arial"
                                                      :fontSize 48
                                                      :fill "white"})]
                          (.. text -position (set 400 300))
                          (.. text -anchor (set 0.5))
                          (set! (.. text -visible) false)
                          text))

(defonce paused-text (let [text (js/PIXI.Text. "P A U S E D"
                                               #js{:fontFamily "Arial"
                                                   :fontSize 48
                                                   :fill "white"})]
                       (.. text -position (set 400 300))
                       (.. text -anchor (set 0.5))
                       (set! (.. text -visible) false)
                       text))

(def water-sprite (js/PIXI.Sprite.
                   (js/PIXI.Texture.
                    (js/PIXI.BaseTexture.fromImage
                     "textures/flood_control/background.png"))))
(set! (.. water-sprite -alpha) 0.7)

(def board-width  8)
(def board-height 10)

(def max-playable-piece-index 5)

(def empty-piece {:piece-type :empty
                  :suffixes #{}})

(def min-time-since-last-input 0.4)
(def max-flood-counter 100.0)
(def max-water-height 244)
(def water-width 297)
(def flood-acceleration-per-level 0.5)

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

(def water-overlay-start {:x 85  :y 245})
(def water-position      {:x 478 :y 340})
(def score-position      {:x 608 :y 245})
(def level-text-position {:x 514 :y 245})

(def time-between-flood-increase 1)

(def fall-rate         5)
(def rotation-rate     9)
(def alpha-change-rate 0.02)

(defn new-board []
  (swap! context assoc :board
         (mapv (fn [_]
                 (mapv (fn [_])
                       (range board-height)))
               (range board-width))))

(defn clear-board []
  (doseq [x (range board-width)]
    (doseq [y (range board-height)]
      (swap! context update-in [:board x y] merge empty-piece))))

(defn locked? [x y]
  (get-in @context [:board x y :suffixes :l]))

(defn get-source-rect [piece]
  (js/PIXI.Rectangle. (+ texture-offset-x
                         (if (get-in piece [:suffixes :w])
                           (+ piece-width texture-padding-x)
                           0)
                         (if (get-in piece [:suffixes :l])
                           (* 2 (+ piece-width texture-padding-x))
                           0))
                      (+ texture-offset-y
                         (* (piece-types (:piece-type piece))
                            (+ piece-height texture-padding-y)))
                      piece-width
                      piece-height))

(defn sprite [piece-type]
  (let [texture (js/PIXI.Texture.fromImage "textures/flood_control/tile_sheet.png")
        rect (get-source-rect {:piece-type piece-type})]
    (js/PIXI.Sprite. (js/PIXI.Texture. texture rect rect))))

(defn update-sprite [piece]
  (let [sprite (:sprite piece)
        rect (get-source-rect piece)]
    (set! (.. sprite -texture -orig) rect)
    (set! (.. sprite -texture -frame) rect)
    (.. sprite -texture (_updateUvs))))

(defn fill-empty-pieces []
  (let [texture (js/PIXI.Texture.fromImage "textures/flood_control/tile_sheet.png")]
    (doseq [x (range board-width)]
      (doseq [y (range board-height)]
        (let [px (+ game-board-display-position-x (* x piece-width))
              py (+ game-board-display-position-y (* y piece-height))
              rect (get-source-rect {:piece-type :empty})
              empty-sprite (js/PIXI.Sprite. (js/PIXI.Texture. texture rect rect))
              sprite       (js/PIXI.Sprite. (js/PIXI.Texture. texture rect rect))]
          (.. board (addChild empty-sprite))
          (.. board (addChild sprite))
          (.. empty-sprite -position (set px py))
          (.. sprite       -position (set px py))
          (set! (.. sprite -visible) false)
          (swap! context assoc-in [:board x y :sprite] sprite))))))

(defn add-falling-piece [x y v-offset]
  (let [piece (get-in @context [:board x y])
        suffix (get-in piece [:suffixes :l])
        piece-type (:piece-type piece)
        sprite (sprite piece-type)]
    (.. board (addChild sprite))
    (swap! context assoc-in [:falling-pieces {:x x :y y}]
           {:piece-type piece-type
            :v-offset v-offset
            :sprite sprite
            :suffixes (cond-> #{} suffix (conj suffix))})))

(defn add-rotating-piece [x y clockwise?]
  (when-not (locked? x y)
    (let [piece (get-in @context [:board x y])
          suffix (get-in piece [:suffixes :l])
          piece-type (:piece-type piece)
          sprite (sprite piece-type)
          px (+ game-board-display-position-x (* x piece-width) (/ piece-width 2))
          py (+ game-board-display-position-y (* y piece-height) (/ piece-height 2))]
      (.. board (addChild sprite))
      (.. sprite -anchor (set 0.5))
      (.. sprite -position (set px py))
      (swap! context assoc-in [:rotating-pieces {:x x :y y}]
             {:piece-type piece-type
              :clockwise? clockwise?
              :rotation-ticks-remaining 10
              :rotation-amount 0
              :sprite sprite
              :suffixes (cond-> #{} suffix (conj suffix))}))))

(defn add-fading-piece [x y]
  (let [piece (get-in @context [:board x y])
        suffix (get-in piece [:suffixes :l])
        piece-type (:piece-type piece)]
    (swap! context assoc-in [:fading-pieces {:x x :y y}]
           {:piece-type piece-type
            :alpha-level 1.0
            :suffixes (cond-> #{:w} suffix (conj suffix))})))

(defn random-piece [x y]
  (let [types [:left-right :top-bottom :left-top :top-right :right-bottom :bottom-left]
        piece-type (get types (rand-int (count types)))
        suffix (when (< (rand) 0.1) :l)]
    (println (get-in @context [:board x y]))
    (swap! context update-in [:board x y] merge
           {:piece-type piece-type
            :suffixes (cond-> #{} suffix (conj suffix))})))

(defn fill-from-above [x y]
  (loop [row-lookup (dec y)]
    (let [piece-type (get-in @context [:board x row-lookup :piece-type])]
      (when (>= row-lookup 0)
        (if (not= piece-type :empty)
          (let [locked? (locked? x y)]
            (swap! context assoc-in [:board x y :piece-type] piece-type)
            (swap! context assoc-in [:board x y :suffixes] (cond-> #{} locked? (conj :l)))
            (swap! context assoc-in [:board x row-lookup :piece-type] :empty)
            (add-falling-piece x y (* piece-height (- y row-lookup)))
            (recur -1))
          (recur (dec row-lookup)))))))

(defn generate-new-pieces [drop-squares?]
  (when drop-squares?
    (doseq [x (range board-width)]
      (doseq [y (range (dec board-height) -1 -1)]
        (when (= (get-in @context [:board x y :piece-type]) :empty)
          (fill-from-above x y)))))
  (doseq [x (range board-width)]
    (doseq [y (range board-height)]
      (when (= (get-in @context [:board x y :piece-type]) :empty)
        (random-piece x y)
        (add-falling-piece x y (* piece-height board-height))))))

(defn start-new-level []
  (swap! context update :current-level inc)
  (swap! context assoc :flood-count 0)
  (swap! context assoc :lines-completed-this-level 0)
  (swap! context update :flood-increase-amount + flood-acceleration-per-level)
  (clear-board)
  (generate-new-pieces false))

(defn draw-background [textures]
  #_(engine/draw-rectangle
   {:texture (:background textures)}))

(defn draw-board [{:keys [textures board falling-pieces rotating-pieces fading-pieces]}]
  (doseq [x (range board-width)]
    (doseq [y (range board-height)]
      (let [px (+ game-board-display-position-x (* x piece-width))
            py (+ game-board-display-position-y (* y piece-height))]
        #_(engine/draw-rectangle
         {:texture (:tile-sheet textures)
          :position {:x px :y py}
          :tex-coords (get-source-rect {:piece-type :empty})})
        #_(let [drawn? (when-let [piece (get fading-pieces {:x x :y y})]
                       (engine/draw-rectangle
                        {:texture (:tile-sheet textures)
                         :position {:x px :y py}
                         :color (engine/color [1.0 1.0 1.0 (:alpha-level piece)])
                         :tex-coords (get-source-rect piece)})
                       true)
              drawn? (or (when-let [piece (get falling-pieces {:x x :y y})]
                           (engine/draw-rectangle
                            {:texture (:tile-sheet textures)
                             :position {:x px :y (- py (:v-offset piece))}
                             :tex-coords (get-source-rect piece)})
                           true)
                         drawn?)
              drawn? (or (when-let [piece (get rotating-pieces {:x x :y y})]
                           (engine/draw-rectangle
                            {:texture (:tile-sheet textures)
                             :position {:x px :y py}
                             :effect {:type :rotate
                                      :angle (if (:clockwise? piece)
                                               (:rotation-amount piece)
                                               (- 360 (:rotation-amount piece)))}
                             :tex-coords (get-source-rect piece)})
                           true)
                         drawn?)]
          (when-not drawn?
            (engine/draw-rectangle
             {:texture (:tile-sheet textures)
              :position {:x px :y py}
              :tex-coords (get-source-rect (get-in board [x y]))})))))))

(defn update-board [{:keys [textures board falling-pieces rotating-pieces fading-pieces]}]
  (doseq [x (range board-width)]
    (doseq [y (range board-height)]
      (let [px (+ game-board-display-position-x (* x piece-width))
            py (+ game-board-display-position-y (* y piece-height))]
        #_(engine/draw-rectangle
           {:texture (:tile-sheet textures)
            :position {:x px :y py}
            :tex-coords (get-source-rect {:piece-type :empty})})
        (let [drawn? false #_(when-let [piece (get fading-pieces {:x x :y y})]
                               (engine/draw-rectangle
                                {:texture (:tile-sheet textures)
                                 :position {:x px :y py}
                                 :color (engine/color [1.0 1.0 1.0 (:alpha-level piece)])
                                 :tex-coords (get-source-rect piece)})
                               true)
              drawn? (or (when-let [piece (get falling-pieces {:x x :y y})]
                           (.. (:sprite piece) -position (set px (- py (:v-offset piece))))
                           true)
                         drawn?)
              drawn? (or (when-let [piece (get rotating-pieces {:x x :y y})]
                           (set! (.. (:sprite piece) -rotation)
                                 (if (:clockwise? piece)
                                   (/ (* (:rotation-amount piece) Math/PI) 180)
                                   (/ (* (- 360 (:rotation-amount piece)) Math/PI) 180)))
                           true)
                         drawn?)]
          (let [piece (get-in board [x y])]
            (set! (.. (:sprite piece) -visible) (not drawn?))
            (when-not drawn?
              (update-sprite piece))))))))

(defn draw* []
  (let [{:keys [textures state board] :as ctx} @context]
    (case state
      :title-screen
      (do )
      #_(engine/draw-rectangle
       {:texture (:title-screen textures)})

      :playing
      (do
        (draw-background textures)

        (let [water-height (* max-water-height (/ (:flood-count ctx) 100))]
          #_(engine/draw-rectangle
           {:texture (:background textures)
            :position {:x (:x water-position)
                       :y (+ (:y water-position) (- max-water-height water-height))}
            :tex-coords {:x (:x water-overlay-start)
                         :y (+ (:y water-overlay-start) (- max-water-height water-height))
                         :w water-width
                         :h water-height}
            :color (engine/rgb-color [255 255 255 180])}))

        (draw-board ctx)

        #_(doseq [score-zoom (:score-zooms ctx)]
          (engine/draw-text {:position {:x 400 :y 300}
                             :text (:text score-zoom)
                             :color (:color score-zoom)
                             :scale (:scale score-zoom)}))

        #_(engine/draw-text {:text (:player-score ctx)
                           :font {:size 36}
                           :align :start
                           :color (engine/color [0.0 0.0 0.0 1.0])
                           :position score-position})
        #_(engine/draw-text {:text (:current-level ctx)
                           :font {:size 36}
                           :align :start
                           :color (engine/color [0.0 0.0 0.0 1.0])
                           :position level-text-position}))

      :game-over
      (do
        (draw-background textures)
        (draw-board ctx)
        #_(engine/draw-text {:position {:x 400 :y 300}
                           :scale 7
                           :color (engine/color [1.0 0.0 0.0 0.7])
                           :text "GAME OVER"}))
      
      :paused
      (do )
      #_(engine/draw-text {:position {:x 400 :y 300}
                         :scale 7
                         :text "P A U S E D"})

      nil)))

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

    (conj
     (case (get-other-end x y from)
       "left"   (propagate-water (dec x) y "right")
       "right"  (propagate-water (inc x) y "left")
       "top"    (propagate-water x (dec y) "bottom")
       "bottom" (propagate-water x (inc y) "top")

       nil)
     {:x x :y y :locked? (locked? x y)})))

(defn get-water-chain [y]
  (propagate-water 0 y "left"))

(defn reset-water []
  (doseq [x (range board-width)]
    (doseq [y (range board-height)]
      (swap! context update-in [:board x y :suffixes] disj :w))))

(defn are-pieces-animating? []
  (or
   (not (empty? (:falling-pieces @context)))
   (not (empty? (:rotating-pieces @context)))
   (not (empty? (:fading-pieces @context)))))

(defn update-falling-piece [k delta]
  (swap! context update-in [:falling-pieces k :v-offset] (fn [v] (max 0 (- v fall-rate)))))

(defn update-falling-pieces [delta]
  (doseq [k (keys (:falling-pieces @context))]
    (update-falling-piece k delta)
    (when (= (get-in @context [:falling-pieces k :v-offset]) 0)
      (.. (get-in @context [:falling-pieces k :sprite]) destroy)
      (swap! context update :falling-pieces dissoc k))))

(defn update-rotating-piece [k delta]
  (swap! context update-in [:rotating-pieces k :rotation-amount] + rotation-rate)
  (swap! context update-in [:rotating-pieces k :rotation-ticks-remaining]
         (fn [v] (max 0 (dec v)))))

(defn update-rotating-pieces [delta]
  (doseq [k (keys (:rotating-pieces @context))]
    (update-rotating-piece k delta)
    (when (= (get-in @context [:rotating-pieces k :rotation-ticks-remaining]) 0)
      (.. (get-in @context [:rotating-pieces k :sprite]) destroy)
      (swap! context update :rotating-pieces dissoc k))))

(defn update-fading-piece [k]
  (swap! context update-in [:fading-pieces k :alpha-level] - alpha-change-rate)
  (swap! context update-in [:fading-pieces k :alpha-level] max 0))

(defn update-fading-pieces []
  (doseq [k (keys (:fading-pieces @context))]
    (update-fading-piece k)
    (when (= (get-in @context [:fading-pieces k :alpha-level]) 0)
      (swap! context update :fading-pieces dissoc k))))

(defn update-animated-pieces [delta]
  (if (not (empty? (:fading-pieces @context)))
    (update-fading-pieces)
    (do
      (update-falling-pieces delta)
      (update-rotating-pieces delta))))

(defn update-score-zooms [score-zooms]
  (->> score-zooms
       (mapv (fn [score-zoom]
               (-> score-zoom
                   (update :scale + (:last-scale-amount score-zoom) (:scale-amount score-zoom))
                   (update :last-scale-amount + (:scale-amount score-zoom))
                   (update :display-counter inc))))
       (filterv (fn [score-zoom]
                  (not (> (:display-counter score-zoom) (:max-display-count score-zoom)))))))

(defn rotate-piece [x y clockwise?]
  (when-not (locked? x y)
    (let [piece-type (get-in @context [:board x y :piece-type])
          piece-type (or ({[:left-right true]    :top-bottom
                           [:left-right false]   :top-bottom

                           [:top-bottom true]    :left-right
                           [:top-bottom false]   :left-right

                           [:left-top true]      :top-right
                           [:left-top false]     :bottom-left

                           [:top-right true]     :right-bottom
                           [:top-right false]    :left-top

                           [:right-bottom true]  :bottom-left
                           [:right-bottom false] :top-right

                           [:bottom-left true]   :left-top
                           [:bottom-left false]  :right-bottom}
                          [piece-type clockwise?])
                         :empty)]
      (swap! context assoc-in [:board x y :piece-type] piece-type))))

(defn handle-mouse-input []
  (when-let [mouse-state (controls/get-mouse-state)]
    (let [x (Math/floor (/ (- (:x mouse-state) game-board-display-position-x) piece-width))
          y (Math/floor (/ (- (:y mouse-state) game-board-display-position-y) piece-height))]
      (when (and (>= x 0) (< x board-width)
                 (>= y 0) (< y board-height))
        (case (:button mouse-state)
          :left
          (do
            (add-rotating-piece x y true)
            (rotate-piece x y true))

          :right
          (do
            (add-rotating-piece x y false)
            (rotate-piece x y false))

          nil)
        (swap! context assoc :time-since-last-input 0)))))

(defn handle-touch-input []
  (when-let [touch-state (controls/get-touch-state)]
    (let [x (Math/floor (/ (- (:x touch-state) game-board-display-position-x) piece-width))
          y (Math/floor (/ (- (:y touch-state) game-board-display-position-y) piece-height))]
      (when (and (>= x 0) (< x board-width)
                 (>= y 0) (< y board-height))
        (add-rotating-piece x y true)
        (rotate-piece x y true)
        (swap! context assoc :time-since-last-input 0)))))

(defn clamp [v min-v max-v]
  (min max-v (max v min-v)))

(defn update* [delta]
  (let [{:keys [state] :as ctx} @context]
    (case state
      :title-screen
      (when (or (controls/key-pressed? :Space) (controls/get-touch-state))
        (set! (.. title-screen -visible) false)
        (set! (.. board -visible) true)
        (clear-board)
        (swap! context assoc :player-score 0)
        (swap! context assoc :current-level 0)
        (swap! context assoc :flood-increase-amount 0)
        (swap! context assoc :state :playing)
        (start-new-level))

      :playing
      (do
        (swap! context update :time-since-last-input + (* delta 0.001))
        (swap! context update :time-since-last-flood-increase + (* delta 0.001))

        (when (>= (:time-since-last-flood-increase @context) time-between-flood-increase)
          (swap! context update :flood-count + (:flood-increase-amount @context))
          (swap! context assoc :time-since-last-flood-increase 0)
          (when (>= (:flood-count @context) max-flood-counter)
            (swap! context assoc :game-over-timer 8)
            (swap! context assoc :state :game-over)))
        
        (if (are-pieces-animating?)
          (update-animated-pieces delta)
          (do
            (reset-water)

            (doseq [y (range board-height)]
              (when-let [chain (get-water-chain y)]
                (let [last-pipe (last chain)]
                  (when (and (= (:x last-pipe) (dec board-width))
                             (has-connector? (:x last-pipe) (:y last-pipe) "right"))
                    (let [piece-count (count chain)
                          locked-water-pieces (count (filter :locked? chain))
                          score (Math/floor
                                 (* (+ (Math/pow (/ piece-count 5) 2)
                                       piece-count
                                       (Math/pow (/ locked-water-pieces 5) 2)
                                       locked-water-pieces)
                                    10))]
                      (swap! context update :player-score + score)
                      (swap! context update :lines-completed-this-level inc)
                      (swap! context assoc :flood-count (clamp (- (:flood-count @context) (/ score 10)) 0 100))
                      (swap! context update :score-zooms conj {:text (str "+" score)
                                                               :max-display-count 30
                                                               :display-counter 0
                                                               :scale 0.4
                                                               :last-scale-amount 0.0
                                                               :scale-amount 0.4
                                                               #_:color #_(engine/color [1.0 0.0 0.0 0.4])})

                      (doseq [scoring-square chain]
                        (let [x (:x scoring-square)
                              y (:y scoring-square)]
                          (add-fading-piece x y)
                          (swap! context assoc-in [:board x y :piece-type] :empty)))

                      (when (>= (:lines-completed-this-level @context) 10)
                        (start-new-level)))))))

            (generate-new-pieces true)

            (when (>= (:time-since-last-input @context) min-time-since-last-input)
              (handle-mouse-input)
              (handle-touch-input))))

        (swap! context update :score-zooms update-score-zooms)

        (when (and (>= (:time-since-last-input @context) min-time-since-last-input)
                   (controls/key-pressed? :KeyP))
          (set! (.. board -visible) false)
          (set! (.. paused-text -visible) true)
          (swap! context assoc :time-since-last-input 0)
          (swap! context assoc :state :paused))

        (let [water-height (* max-water-height (/ (:flood-count ctx) 100))]
          (set! (.. water-sprite -texture -orig)
                (js/PIXI.Rectangle.
                 (:x water-overlay-start)
                 (+ (:y water-overlay-start) (- max-water-height water-height))
                 water-width
                 water-height))
          (set! (.. water-sprite -texture -frame)
                (js/PIXI.Rectangle.
                 (:x water-overlay-start)
                 (+ (:y water-overlay-start) (- max-water-height water-height))
                 water-width
                 water-height))
          (.. water-sprite -position (set (:x water-position)
                                          (+ (:y water-position) (- max-water-height water-height))))
          (.. water-sprite -texture (_updateUvs)))

        (update-board @context))

      :game-over
      (do
        (swap! context update :game-over-timer - (* delta 0.001))
        (when (<= (:game-over-timer @context) 0)
          (swap! context assoc :state :title-screen)))

      :paused
      (do
        (swap! context update :time-since-last-input + (* delta 0.001))
        (when (and (>= (:time-since-last-input @context) min-time-since-last-input)
                   (controls/key-pressed? :KeyP))
          (set! (.. board -visible) true)
          (set! (.. paused-text -visible) false)
          (swap! context assoc :time-since-last-input 0)
          (swap! context assoc :state :playing)))

      nil)))

(defn texture [tex-name]
  (str "textures/flood_control/" tex-name))

(defn init []
  (when-not (:initialized? @context)
    (.. root (addChild title-screen))
    (.. root (addChild board))
    (.. root (addChild paused-text))
    (.. root (addChild game-over-text))

    (set! (.. board -visible) false)

    (.. board (addChild background))
    (.. board (addChild water-sprite))
    (game/run (pixi/init
               ["textures/flood_control/title_screen.png"
                "textures/flood_control/background.png"
                "textures/flood_control/tile_sheet.png"]
               #(do
                  (new-board)
                  (fill-empty-pieces))) update* root)
    (swap! context assoc :initialized? true))
  #_(engine/init {:draw-fn   draw*
                :update-fn update*
                :show-fps? true})

  #_(swap! context assoc-in [:textures :title-screen] (engine/load-texture (texture "title_screen.png")))
  #_(swap! context assoc-in [:textures :background]   (engine/load-texture (texture "background.png")))
  #_(swap! context assoc-in [:textures :tile-sheet]   (engine/load-texture (texture "tile_sheet.png"))))
