(ns games.gemstone-hunter.game-object
  (:require [clojure.string :as str]
            [games.engine :as engine]
            [games.gemstone-hunter.utils :as u]
            [games.gemstone-hunter.camera :as camera]
            [games.gemstone-hunter.tile-map :as tile-map]
            [games.gemstone-hunter.animation-strip :as anim]))

(defn world-center [game-object]
  {:x (+ (-> game-object :world-location :x) (/ (:frame-width game-object) 2))
   :y (+ (-> game-object :world-location :y) (/ (:frame-height game-object) 2))})

(defn world-rectangle [game-object]
  {:x (-> game-object :world-location :x)
   :y (-> game-object :world-location :y)
   :width (:frame-width game-object)
   :height (:frame-height game-object)})

(defn collision-rectangle [game-object]
  {:x (+ (-> game-object :world-location :x) (-> game-object :collision-rectangle :x))
   :y (+ (-> game-object :world-location :y) (-> game-object :collision-rectangle :y))
   :width (-> game-object :collision-rectangle :width)
   :height (-> game-object :collision-rectangle :height)})

(defn play-animation [game-object name]
  (if (str/blank? name)
    game-object
    (if (get (:animations game-object) name)
      (-> game-object
          (update-in [:animations (:current-animation game-object)] anim/stop)
          (assoc :current-animation name)
          (update-in [:animations name] anim/play))
      game-object)))

(defn update-animation [game-object elapsed]
  (let [cur (:current-animation game-object)
        animation (get (:animations game-object) cur)]
    (if (:finished-playing? animation)
      (play-animation game-object (:next-animation animation))
      (update-in game-object [:animations cur] anim/update* elapsed))))

(defn horizontal-collision-test [move-amount game-object]
  (if (= (:x move-amount) 0)
    move-amount
    (let [after-move-rect (-> (collision-rectangle game-object)
                              (update :x + (:x move-amount)))
          corner1 (if (< (:x move-amount) 0)
                    {:x (u/rectangle-left after-move-rect) :y (inc (u/rectangle-top after-move-rect))}
                    {:x (u/rectangle-right after-move-rect) :y (inc (u/rectangle-top after-move-rect))})
          corner2 (if (< (:x move-amount) 0)
                    {:x (u/rectangle-left after-move-rect) :y (dec (u/rectangle-bottom after-move-rect))}
                    {:x (u/rectangle-right after-move-rect) :y (dec (u/rectangle-bottom after-move-rect))})
          map-cell1 (tile-map/get-cell-by-pixel corner1)
          map-cell2 (tile-map/get-cell-by-pixel corner2)]
      (if (or (not (tile-map/cell-passable? map-cell1))
              (not (tile-map/cell-passable? map-cell2)))
        (assoc move-amount :x 0)
        (if (:code-based-blocks? game-object)
          (if (or (= (tile-map/cell-code-value map-cell1) "BLOCK")
                  (= (tile-map/cell-code-value map-cell2) "BLOCK"))
            (assoc move-amount :x 0)
            move-amount)
          move-amount)))))

(def on-ground? (atom false))

(defn vertical-collision-test [move-amount game-object]
  (if (= (:y move-amount) 0)
    move-amount
    (let [after-move-rect (-> (collision-rectangle game-object)
                              (update :x + (:x move-amount))
                              (update :y + (:y move-amount)))
          corner1 (if (< (:y move-amount) 0)
                    {:x (inc (u/rectangle-left after-move-rect)) :y (u/rectangle-top after-move-rect)}
                    {:x (inc (u/rectangle-left after-move-rect)) :y (u/rectangle-bottom after-move-rect)})
          corner2 (if (< (:y move-amount) 0)
                    {:x (dec (u/rectangle-right after-move-rect)) :y (u/rectangle-top after-move-rect)}
                    {:x (dec (u/rectangle-right after-move-rect)) :y (u/rectangle-bottom after-move-rect)})
          map-cell1 (tile-map/get-cell-by-pixel corner1)
          map-cell2 (tile-map/get-cell-by-pixel corner2)]
      (if (or (not (tile-map/cell-passable? map-cell1))
              (not (tile-map/cell-passable? map-cell2)))
        (do
          (when (> (:y move-amount) 0)
            (reset! on-ground? true))
          (assoc move-amount :y 0))
        (if (:code-based-blocks? game-object)
          (if (or (= (tile-map/cell-code-value map-cell1) "BLOCK")
                  (= (tile-map/cell-code-value map-cell2) "BLOCK"))
            (do
              (when (> (:y move-amount) 0)
                (reset! on-ground? true))
              (assoc move-amount :y 0))
            move-amount)
          move-amount)))))

(defn update* [game-object elapsed]
  (if (:enabled? game-object)
    (let [game-object (update-animation game-object elapsed)
          _ (reset! on-ground? (if (= (-> game-object :velocity :y) 0)
                                 (:on-ground? game-object)
                                 false))
          game-object (if (= (-> game-object :velocity :y) 0)
                        game-object
                        (assoc game-object :on-ground? false))
          move-amount (-> (:velocity game-object)
                          (u/vector-mul elapsed)
                          (horizontal-collision-test game-object)
                          (vertical-collision-test game-object))
          new-position (u/vector-add (:world-location game-object) move-amount)
          new-position {:x (u/clamp (:x new-position)
                                    0
                                    (- (:width (camera/world-rectangle)) (:frame-width game-object)))
                        :y (u/clamp (:y new-position)
                                    (* 2 (- tile-map/tile-height))
                                    (- (:height (camera/world-rectangle)) (:frame-height game-object)))}
          velocity (:velocity game-object)
          velocity (if (= (:x move-amount) 0)
                     (assoc velocity :x 0)
                     velocity)
          velocity (if (= (:y move-amount) 0)
                     (assoc velocity :y 0)
                     velocity)]
      (when-let [sprite (:draw (get (:animations game-object) (:current-animation game-object)))]
        (case (:flipped? game-object)
          true  (set! (.. sprite -scale -x) -1)
          false (set! (.. sprite -scale -x) 1)
          nil)
        (.. sprite -position (set (+ (:x new-position) (/ (.. sprite -width) 2))
                                  (:y new-position))))
      (assoc game-object
             :on-ground? @on-ground?
             :world-location new-position
             :velocity velocity))
    game-object))

(defn new-game-object []
  {:world-location {:x 0 :y 0}
   :velocity {:x 0 :y 0}
   :frame-width 0
   :frame-height 0
   :enabled? false
   :flipped? false
   :on-ground? false
   :collision-rectangle {:x 0 :y 0 :width 0 :height 0}
   :collide-width 0
   :collide-height 0
   :code-based-blocks? true
   :draw-depth 5
   :animations {}
   :current-animation nil})
