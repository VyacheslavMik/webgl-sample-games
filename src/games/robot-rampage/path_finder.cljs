(ns games.robot-rampage.path-finder
  (:require [games.robot-rampage.tile-map :as tile-map]
            [games.robot-rampage.storage :as s]
            [games.robot-rampage.utils :as u]))

(def cost-straight 10)
(def cost-diagonal 15)

(defn wall? [ctx x y]
  (>= (aget (.-tiles ctx) x y) tile-map/wall-tile-start))

(defn total-cost [end_node grid_location direct_cost]
  (if end_node
    (+ direct_cost
       (Math/sqrt (+ (* (- (.. end_node -grid_location -x) (.-x grid_location))
                        (- (.. end_node -grid_location -x) (.-x grid_location)))
                     (* (- (.. end_node -grid_location -y) (.-y grid_location))
                        (- (.. end_node -grid_location -y) (.-y grid_location))))))
    0))

(defn new-path-node [parent_node end_node grid_location cost total_cost]
  (let [res #js {:parent_node parent_node
                 :end_node    end_node
                 :total_cost  total_cost
                 :direct_cost cost
                 :grid_location grid_location}]
    res))

(defn eq? [node1 node2]
  (and (identical? (.. node1 -grid_location -x) (.. node2 -grid_location -x))
       (identical? (.. node1 -grid_location -y) (.. node2 -grid_location -y))))

(defn hs-set [hs k v]
  (when-not (aget hs (.-x k))
    (aset hs (.-x k) (array)))
  (aset hs (.-x k) (.-y k) v))

(defn hs-get [hs k]
  (when-let [a (aget hs (.-x k))]
    (aget a (.-y k))))

(defn add-node-to-open-list [ctx node]
  (set! (.-next node) nil)
  (if (.. ctx -open_list)
    (if (< (.-total_cost node) (.. ctx -open_list -total_cost))
      (do
        (set! (.-next node) (.-open_list ctx))
        (set! (.-open_list ctx) node))
      (loop [cur (.-open_list ctx)]
        (if (.-next cur)
          (if (< (.-total_cost node) (.. cur -next -total_cost))
            (do
              (set! (.-next node) (.-next cur))
              (set! (.-next cur) node))
            (recur (.-next cur)))
          (set! (.-next cur) node))))
    (set! (.-open_list ctx) node))

  (hs-set (.-node_costs ctx)  (.-grid_location node) (.-total_cost node))
  (hs-set (.-node_status ctx) (.-grid_location node) 1))

(defn find-adjacent-nodes [ctx current-node end_node]
  (let [x (.. current-node -grid_location -x)
        y (.. current-node -grid_location -y)]
    (set! (.-up_left ctx) true)
    (set! (.-up_right ctx) true)
    (set! (.-down_left ctx) true)
    (set! (.-down_right ctx) true)
    (set! (.-status ctx) 0)
    (set! (.-total_cost ctx) 0)
    (set! (.-direct_cost ctx) 0)

    (if (and (> x 0) (not (wall? ctx (dec x) y)))
      (do
        (set! (.-grid_location ctx) #js {:x (dec x) :y y})
        (set! (.-direct_cost ctx) (+ cost-straight (.-direct_cost current-node)))
        (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx) (.-direct_cost ctx)))
        (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
        (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                             (< (.. ctx -total_cost)
                                                (hs-get (.-node_costs ctx)
                                                        (.. ctx -grid_location)))))
          (add-node-to-open-list ctx (new-path-node current-node end_node
                                                    (.-grid_location ctx)
                                                    (.-direct_cost ctx)
                                                    (.-total_cost ctx)))))
      (do
        (set! (.-up_left ctx) false)
        (set! (.-down_left ctx) false)))

    (if (and (< x 49) (not (wall? ctx (inc x) y)))
      (do
        (set! (.-grid_location ctx) #js {:x (inc x) :y y})
        (set! (.-direct_cost ctx) (+ cost-straight (.-direct_cost current-node)))
        (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx) (.-direct_cost ctx)))
        (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
        (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                             (< (.. ctx -total_cost)
                                                (hs-get (.-node_costs ctx)
                                                        (.. ctx -grid_location)))))
          (add-node-to-open-list ctx (new-path-node current-node end_node
                                                    (.-grid_location ctx)
                                                    (.-direct_cost ctx)
                                                    (.-total_cost ctx)))))
      (do
        (set! (.-up_right ctx) false)
        (set! (.-down_right ctx) false)))

    (if (and (> y 0) (not (wall? ctx x (dec y))))
      (do
        (set! (.-grid_location ctx) #js {:x x :y (dec y)})
        (set! (.-direct_cost ctx) (+ cost-straight (.-direct_cost current-node)))
        (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx) (.-direct_cost ctx)))
        (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
        (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                             (< (.. ctx -total_cost)
                                                (hs-get (.-node_costs ctx)
                                                        (.. ctx -grid_location)))))
          (add-node-to-open-list ctx (new-path-node current-node end_node
                                                    (.-grid_location ctx)
                                                    (.-direct_cost ctx)
                                                    (.-total_cost ctx)))))
      (do
        (set! (.-up_left ctx) false)
        (set! (.-up_right ctx) false)))

    (if (and (< y 49) (not (wall? ctx x (inc y))))
      (do
        (set! (.-grid_location ctx) #js {:x x :y (inc y)})
        (set! (.-direct_cost ctx) (+ cost-straight (.-direct_cost current-node)))
        (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx) (.-direct_cost ctx)))
        (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
        (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                             (< (.. ctx -total_cost)
                                                (hs-get (.-node_costs ctx)
                                                        (.. ctx -grid_location)))))
          (add-node-to-open-list ctx (new-path-node current-node end_node
                                                    (.-grid_location ctx)
                                                    (.-direct_cost ctx)
                                                    (.-total_cost ctx)))))
      (do
        (set! (.-down_left ctx) false)
        (set! (.-down_right ctx) false)))

    (when (and (.-up_left ctx) (not (wall? ctx (dec x) (dec y))))
      (set! (.-grid_location ctx) #js {:x (dec x) :y (dec y)})
      (set! (.-direct_cost ctx) (+ cost-diagonal (.-direct_cost current-node)))
      (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx) (.-direct_cost ctx)))
      (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
      (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                           (< (.. ctx -total_cost)
                                              (hs-get (.-node_costs ctx)
                                                      (.. ctx -grid_location)))))
        (add-node-to-open-list ctx (new-path-node current-node end_node
                                                  (.-grid_location ctx)
                                                  (.-direct_cost ctx)
                                                  (.-total_cost ctx)))))

    (when (and (.-up_right ctx) (not (wall? ctx (inc x) (dec y))))
      (set! (.-grid_location ctx) #js {:x (inc x) :y (dec y)})
      (set! (.-direct_cost ctx) (+ cost-diagonal (.-direct_cost current-node)))
      (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx) (.-direct_cost ctx)))
      (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
      (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                           (< (.. ctx -total_cost)
                                              (hs-get (.-node_costs ctx)
                                                      (.. ctx -grid_location)))))
        (add-node-to-open-list ctx (new-path-node current-node end_node
                                                  (.-grid_location ctx)
                                                  (.-direct_cost ctx)
                                                  (.-total_cost ctx)))))

    (when (and (.-down_left ctx) (not (wall? ctx (dec x) (inc y))))
      (set! (.-grid_location ctx) #js {:x (dec x) :y (inc y)})
      (set! (.-direct_cost ctx) (+ cost-diagonal (.-direct_cost current-node)))
      (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx) (.-direct_cost ctx)))
      (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
      (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                           (< (.. ctx -total_cost)
                                              (hs-get (.-node_costs ctx)
                                                      (.. ctx -grid_location)))))
        (add-node-to-open-list ctx (new-path-node current-node end_node
                                                  (.-grid_location ctx)
                                                  (.-direct_cost ctx)
                                                  (.-total_cost ctx)))))

    (when (and (.-down_right ctx) (not (wall? ctx (inc x) (inc y))))
      (set! (.-grid_location ctx) #js {:x (inc x) :y (inc y)})
      (set! (.-direct_cost ctx) (+ cost-diagonal (.-direct_cost current-node)))
      (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx) (.-direct_cost ctx)))
      (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
      (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                           (< (.. ctx -total_cost)
                                              (hs-get (.-node_costs ctx)
                                                      (.. ctx -grid_location)))))
        (add-node-to-open-list ctx (new-path-node current-node end_node
                                                  (.-grid_location ctx)
                                                  (.-direct_cost ctx)
                                                  (.-total_cost ctx)))))))

(defn find-path [start-tile end-tile]
  (let [tiles (:map-squares @s/context)
        ctx   #js {:tiles tiles}]
    (when (not (or (wall? ctx (:x start-tile) (:y start-tile))
                   (wall? ctx (:x end-tile)   (:y end-tile))))
      (let [end_node (new-path-node nil nil #js {:x (:x end-tile) :y (:y end-tile)} 0 0)
            grid_location #js {:x (:x start-tile) :y (:y start-tile)}
            total_cost (total-cost end_node grid_location 0)
            start-node (new-path-node nil end_node grid_location 0 total_cost)
            tmp (atom 0)]
        (set! (.-open_list ctx) nil)
        (set! (.-node_costs ctx) (array))
        (set! (.-node_status ctx) (array))
        (add-node-to-open-list ctx start-node)
        (loop []
          (swap! tmp inc)
          (if (> @tmp 10000)
            (println "Infinite loop")
            (when (.. ctx -open_list)
              (let [current-node (.. ctx -open_list)]
                (set! (.-open_list ctx) (.-next current-node))
                (set! (.-next current-node) nil)

                (if (eq? current-node end_node)
                  (do
                    (loop [res nil
                           cn current-node]
                      (if (.-parent_node cn)
                        (recur (.-grid_location cn) (.-parent_node cn))
                        {:x (.. res -x) :y (.. res -y)})))
                  (do
                    (hs-set (.-node_costs ctx) (.-grid_location current-node) nil)
                    (find-adjacent-nodes ctx current-node end_node)
                    (hs-set (.-node_status ctx) (.-grid_location current-node) 0)
                    (recur)))))))))))
