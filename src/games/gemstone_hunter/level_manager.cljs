(ns games.gemstone-hunter.level-manager
  (:require [goog.string :as gstring]
            [goog.string.format]
            [games.gemstone-hunter.utils :as u]
            [games.gemstone-hunter.player :as player]
            [games.gemstone-hunter.gemstone :as gem]
            [games.gemstone-hunter.enemy :as enemy]
            [games.gemstone-hunter.game-object :as game-object]
            [games.gemstone-hunter.world :as world]
            [games.gemstone-hunter.animation-strip :as anim]
            [games.gemstone-hunter.tile-map :as tile-map]))

(def context (atom {}))

(defn enemy-type []
  (case (rand-int 4)
    0 "A"
    1 "B"
    2 "C"
    3 "D"))

(defn remove-object [game-object]
  (doseq [[_ animation] (:animations game-object)]
    (anim/stop animation)))

(defn load-level [level-number]
  (swap! context assoc :loading? true)
  (-> (js/fetch (gstring/format "maps/gemstone_hunter/MAP%03d" level-number))
      (.then (fn [response]
               (.text response)))
      (.then (fn [s]
               (swap! context assoc :loading? false)
               (world/clear)
               (tile-map/load-map s)
               (swap! context assoc :gemstones [])
               (swap! context assoc :enemies [])
               (dotimes [x tile-map/map-width]
                 (dotimes [y tile-map/map-height]
                   (let [code (tile-map/cell-code-value x y)]
                     (when (= code "START")
                       (swap! context update :player
                              (fn [player]
                                (-> player
                                    (assoc :world-location {:x (* x tile-map/tile-width)
                                                            :y (* y tile-map/tile-height)})
                                    (player/play-animation "idle")))))
                     (when (= code "GEM")
                       (swap! context update :gemstones conj (gem/new-gemstone x y)))
                     (when (= code "ENEMY")
                       (swap! context update :enemies conj (enemy/new-enemy x y (enemy-type)))))))
               (swap! context assoc :current-level level-number)
               (swap! context assoc :respawn-location (-> @context :player :world-location))))))

(defn init []
  (swap! context assoc :player (player/new-player load-level context)))

(defn check-current-cell-code [player]
  (if (:dead? player)
    player
    (let [code (tile-map/cell-code-value (tile-map/get-cell-by-pixel (:world-location player)))]
      (if (= code "DEAD")
        (player/kill player)
        player))))

(defn update-gemstones [player gemstones elapsed]
  (let [gemstones (mapv (fn [gem] (game-object/update* gem elapsed)) gemstones)
        c (count gemstones)
        gemstones (filterv (fn [gem]
                             (let [alive? (not
                                           (u/rectangle-intersects?
                                            (game-object/collision-rectangle player)
                                            (game-object/collision-rectangle gem)))]
                               (when-not alive?
                                 (remove-object gem))
                               alive?))
                           gemstones)
        score (- c (count gemstones))]
    {:gemstones gemstones
     :player (update player :score +  (* score 10))}))

(defn update-enemies [player enemies elapsed]
  (let [enemies (mapv (fn [enemy]
                        (enemy/update* enemy elapsed))
                      enemies)]
    (reduce (fn [acc enemy]
              (if (:dead? enemy)
                (if (:enabled? enemy)
                  (update acc :enemies conj enemy)
                  (do
                    (remove-object enemy)
                    acc))
                (if (u/rectangle-intersects?
                     (game-object/collision-rectangle player)
                     (game-object/collision-rectangle enemy))
                  (if (< (:y (game-object/world-center (:player acc)))
                         (-> enemy :world-location :y))
                    (let [player (-> (:player acc)
                                     (player/jump)
                                     (update :score + 5))
                          enemy (-> enemy
                                    (game-object/play-animation "die")
                                    (assoc :dead? true
                                           :velocity {:x 0 :y 0}))]
                      (-> acc
                          (assoc :player player)
                          (update :enemies conj enemy)))
                    (-> acc
                        (assoc :player (player/kill player))
                        (update :enemies conj enemy)))
                  (update acc :enemies conj enemy))))
            {:player player :enemies []} enemies)))

(defn update* [elapsed]
  (when-not (:loading? @context)
    (let [{:keys [player gemstones enemies]} @context
          player (-> (player/update* player elapsed)
                     (check-current-cell-code))
          {:keys [player gemstones]} (update-gemstones player gemstones elapsed)
          {:keys [player enemies]}   (update-enemies   player enemies   elapsed)]
      (swap! context assoc :player player)
      (swap! context assoc :gemstones gemstones)
      (swap! context assoc :enemies enemies))))

(defn reload-level []
  (let [save-respawn (:respawn-location @context)]
    (load-level (:current-level @context))
    (swap! context assoc :respawn-location save-respawn)
    (swap! context update-in [:player :world-location] save-respawn)))

(defn revive-player []
  (swap! context update :player player/revive))
