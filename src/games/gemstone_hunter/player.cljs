(ns games.gemstone-hunter.player
  (:require [games.engine :as engine]
            [games.gemstone-hunter.utils :as u]
            [games.gemstone-hunter.animation-strip :as anim]
            [games.gemstone-hunter.camera :as camera]
            [games.gemstone-hunter.game-object :as game-object]))

(defn load-texture [name]
  (engine/load-texture (str "textures/gemstone_hunter/Sprites/Player/" name)))

(defn new-player []
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
               :code-based-blocks? false)
        (game-object/play-animation "idle"))))

(defn play-animation [player animation]
  (if (= (:current-animation player) animation)
    player
    (game-object/play-animation player animation)))

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
          (play-animation new-animation)))))

(defn reposition-camera [player]
  (let [x (:x (camera/world-to-screen-v (:world-location player)))]
    (when (> x 500)
      (camera/move {:x (- x 500) :y 0}))
    (when (< x 200)
      (camera/move {:x (- x 200) :y 0}))
    player))

(defn update* [player elapsed]
  (-> player
      (not-dead-update elapsed)
      (update :velocity u/vector-add (:fall-speed player))
      (reposition-camera)
      (game-object/update* elapsed)))
