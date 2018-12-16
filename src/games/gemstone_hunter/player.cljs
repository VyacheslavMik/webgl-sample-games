(ns games.gemstone-hunter.player
  (:require [clojure.string :as str]
            [games.engine :as engine]
            [games.controls :as controls]
            [games.gemstone-hunter.utils :as u]
            [games.gemstone-hunter.camera :as camera]
            [games.gemstone-hunter.animation-strip :as anim]
            [games.gemstone-hunter.game-object :as game-object]
            [games.gemstone-hunter.world :as world]
            [games.gemstone-hunter.tile-map :as tile-map]))

(defn texture-path [name]
  (str "textures/gemstone_hunter/Sprites/Player/" name))

(defn new-player [load-level context]
  (let [ob (game-object/new-game-object)
        animations {"idle" (-> (anim/new-animation-strip (texture-path "Idle.png") 48 "idle")
                               (assoc :loop-animation? true)
                               (anim/update-animation-strip))
                    "run" (-> (anim/new-animation-strip (texture-path "Run.png") 48 "run")
                              (assoc :loop-animation? true)
                              (anim/update-animation-strip))
                    "jump" (-> (anim/new-animation-strip (texture-path "Jump.png") 48 "jump")
                               (assoc :loop-animation? false
                                      :frame-delay 0.2
                                      :next-animation "idle")
                               (anim/update-animation-strip))
                    "die" (-> (anim/new-animation-strip (texture-path "Die.png") 48 "die")
                              (assoc :loop-animation? false)
                              (anim/update-animation-strip))}]
    (-> ob
        (assoc :animations animations
               :world-location {:x -500 :y -500}
               :fall-speed {:x 0 :y 20}
               :move-scale 180
               :dead? false
               :score 0
               :lives-remaining 3
               :frame-width 48
               :frame-height 48
               :collision-rectangle {:x 9 :y 1 :width 30 :height 46}
               :draw-depth 6
               :enabled? true
               :load-level load-level
               :context context
               :code-based-blocks? false))))

(defn play-animation [player animation]
  (if (= (:current-animation player) animation)
    player
    (game-object/play-animation player animation)))

(defn check-level-transition [player]
  (if (controls/key-pressed? :KeyW)
    (let [center-cell (tile-map/get-cell-by-pixel (game-object/world-center player))
          code (tile-map/cell-code-value center-cell)]
      (if (and code (str/starts-with? code "T_"))
        (let [code (str/split code #"_")]
          (if (= (count code) 4)
            (let [loc {:x (js/parseInt (* (nth code 2) tile-map/tile-width))
                       :y (js/parseInt (* (nth code 3) tile-map/tile-height))}]
              ((:load-level player) (js/parseInt (nth code 1)))
              (swap! (:context player) assoc :respawn-location loc)
              (assoc player
                     :world-location loc
                     :velocity {:x 0 :y 0}))
            player))
        player))
    player))

(defn touch-right [player]
  (when-let [touch (controls/get-touch-state)]
    (> 0 (- (:x (camera/world-to-screen-v (:world-location player)))
            (:x (first touch))))))

(defn touch-left [player]
  (when-let [touch (controls/get-touch-state)]
    (< 0 (- (:x (camera/world-to-screen-v (:world-location player)))
            (:x (first touch))))))

(defn touch-top [player]
  (when-let [touch (controls/get-touch-state)]
    (< 0 (- (:y (camera/world-to-screen-v (:world-location player)))
            (:y (first touch))))))

(defn not-dead-update [player elapsed]
  (if (:dead? player)
    player
    (let [new-animation (cond
                          (or
                           (controls/key-pressed? :KeyA)
                           (touch-left player))
                          "run"

                          (or
                           (controls/key-pressed? :KeyD)
                           (touch-right player))
                          "run"
                          
                          :else
                          "idle")
          velocity (cond
                     (or
                      (controls/key-pressed? :KeyA)
                      (touch-left player))
                     {:x (- (:move-scale player)) :y (-> player :velocity :y)}

                     (or (controls/key-pressed? :KeyD)
                         (touch-right player))
                     {:x (:move-scale player) :y (-> player :velocity :y)}

                     :else
                     {:x 0 :y (-> player :velocity :y)})

          flipped? (cond
                     (or
                      (controls/key-pressed? :KeyA)
                      (touch-left player))
                     false

                     (or
                      (controls/key-pressed? :KeyD)
                      (touch-right player))
                     true

                     :else
                     (:flipped? player))
          velocity (if (and (or (controls/key-pressed? :Space)
                                (touch-top player))
                            (:on-ground? player))
                     (update velocity :y - 500)
                     velocity)
          new-animation (if (and (or (controls/key-pressed? :Space)
                                     (touch-top player))
                                 (:on-ground? player))
                          "jump"
                          new-animation)
          new-animation (if (= (:current-animation player) "jump") "jump" new-animation)]
      (-> player
          (assoc :flipped? flipped?
                 :velocity velocity)
          (check-level-transition)
          (play-animation new-animation)))))

(defn jump [player]
  (update-in player [:velocity :y] - 500))

(defn reposition-camera [player]
  (let [x (:x (camera/world-to-screen-v (:world-location player)))]
    (when (> x 500)
      (camera/move {:x (- x 500) :y 0}))
    (when (< x 200)
      (camera/move {:x (- x 200) :y 0}))
    player))

(defn kill [player]
  (if (:dead? player)
    player
    (-> player
        (play-animation "die")
        (update :lives-remaining dec)
        (assoc-in [:velocity :x] 0)
        (assoc :dead? true))))

(defn update* [player elapsed]
  (-> player
      (not-dead-update elapsed)
      (update :velocity u/vector-add (:fall-speed player))
      (reposition-camera)
      (game-object/update* elapsed)))

(defn revive [player]
  (-> player
      (game-object/play-animation "idle")
      (assoc :dead? false
             :world-location {:x -500 :y -500})))
