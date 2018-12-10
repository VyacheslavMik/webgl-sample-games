(ns games.gemstone-hunter.player
  (:require [clojure.string :as str]
            [games.engine :as engine]
            [games.gemstone-hunter.utils :as u]
            [games.gemstone-hunter.camera :as camera]
            [games.gemstone-hunter.animation-strip :as anim]
            [games.gemstone-hunter.game-object :as game-object]
            [games.gemstone-hunter.tile-map :as tile-map]))

(defn load-texture [name]
  (engine/load-texture (str "textures/gemstone_hunter/Sprites/Player/" name)))

(defn new-player [load-level context]
  (let [ob (game-object/new-game-object)
        animations {"idle" (-> (anim/new-animation-strip (load-texture "Idle.png")
                                                         48
                                                         "idle")
                               (assoc :loop-animation? true))
                    "run" (-> (anim/new-animation-strip (load-texture "Run.png")
                                                        48
                                                        "run")
                              (assoc :loop-animation? true))
                    "jump" (-> (anim/new-animation-strip (load-texture "Jump.png")
                                                         48
                                                         "jump")
                               (assoc :loop-animation? false
                                      :frame-delay 0.08
                                      :next-animation "idle"))
                    "die" (-> (anim/new-animation-strip (load-texture "Die.png")
                                                        48
                                                        "die")
                              (assoc :loop-animation? false))}]
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
               :code-based-blocks? false)
        (game-object/play-animation "idle"))))

(defn play-animation [player animation]
  (if (= (:current-animation player) animation)
    player
    (game-object/play-animation player animation)))

(defn check-level-transition [player]
  (if (engine/key-pressed? :KeyW)
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

(defn not-dead-update [player elapsed]
  (if (:dead? player)
    player
    (let [new-animation (cond
                          (engine/key-pressed? :KeyA)
                          "run"
                          
                          (engine/key-pressed? :KeyD)
                          "run"
                          
                          :else
                          "idle")
          velocity (cond
                     (engine/key-pressed? :KeyA)
                     {:x (- (:move-scale player)) :y (-> player :velocity :y)}

                     (engine/key-pressed? :KeyD)
                     {:x (:move-scale player) :y (-> player :velocity :y)}

                     :else
                     {:x 0 :y (-> player :velocity :y)})

          flipped? (cond
                     (engine/key-pressed? :KeyA)
                     false
                     
                     (engine/key-pressed? :KeyD)
                     true

                     :else
                     (:flipped? player))
          velocity (if (and (engine/key-pressed? :Space)
                            (:on-ground? player))
                     (update velocity :y - 500)
                     velocity)
          new-animation (if (and (engine/key-pressed? :Space)
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
