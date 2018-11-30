(ns games.robot-rampage.path-finder
  (:require [games.robot-rampage.tile-map :as tile-map]
            [games.robot-rampage.storage :as s]
            [games.robot-rampage.utils :as u]))

(def cost-straight 10)
(def cost-diagonal 15)

(defn wall? [ctx x y]
  (>= (aget (.-tiles ctx) x y) tile-map/wall-tile-start))

(defn total-cost [end_node grid_location]
  (if end_node
    (Math/sqrt (+ (* (- (.-x grid_location) (.. end_node -grid_location -x))
                     (- (.-x grid_location) (.. end_node -grid_location -x)))
                  (* (- (.-y grid_location) (.. end_node -grid_location -y))
                     (- (.-y grid_location) (.. end_node -grid_location -y)))))
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

(defn print-count [ctx]
  (loop [i 0
         s ""
         c (.-open_list ctx)]
    (if c
      (recur (inc i) (str s " " (.-total_cost c)) (.-next c))
      (do
        (println "costs" s)
        (println "open list count" i)))))

#_(defn print-count [ctx]
  (let [s (atom "")]
    (doseq [c (.-open_list ctx)]
      (swap! s str " " (.-total_cost c)))
    (println "costs" @s)
    (println "open list count" (.. ctx -open_list -length)))
  ctx)

(defn add-node-to-open-list [ctx node]
  #_(println "-----------------add" (.-total_cost node))
  #_(print-count ctx)
  (set! (.-next node) nil)
  (if (.. ctx -open_list)
    (if (< (.-total_cost node) (.. ctx -open_list -total_cost))
      (do
        #_(println "top" (.-total_cost node) (.. ctx -open_list -total_cost))
        (set! (.-next node) (.-open_list ctx))
        (set! (.-open_list ctx) node))
      (loop [cur (.-open_list ctx)]
        #_(println (.-total_cost cur))
        (if (.-next cur)
          (if (< (.-total_cost node) (.. cur -next -total_cost))
            (do
              #_(println "middle" (.-total_cost node) (.. cur -next -total_cost))
              (set! (.-next node) (.-next cur))
              (set! (.-next cur) node))
            (recur (.-next cur)))
          (do
            #_(println "last" (.-total_cost node) (.. cur -total_cost) (.-next cur))
            (set! (.-next node) nil)
            (set! (.-next cur) node)))))
    (do
      #_(println "new" (.-total_cost node))
      (set! (.-next node) nil)
      (set! (.-open_list ctx) node)))
  #_(print-count ctx)
  #_(println "-----------------end" (.-total_cost node))

  (hs-set (.-node_costs ctx)  (.-grid_location node) (.-total_cost node))
  (hs-set (.-node_status ctx) (.-grid_location node) 1))


#_(defn add-node-to-open-list [ctx node]
  (let [cost (.-total_cost node)
        c (.. ctx -open_list -length)]
    (loop [index 0]
      (if (and (> c index) (< cost (.-total_cost (aget (.-open_list ctx) index))))
        (recur (inc index))
        (do
          (.splice (.-open_list ctx) index 0 node)
          (hs-set (.-node_costs ctx)  (.-grid_location node) (.-total_cost node))
          (hs-set (.-node_status ctx) (.-grid_location node) 1)
          ctx))))
  (print-count ctx))

(defn find-adjacent-nodes [ctx current-node end_node]
  (let [x (.. current-node -grid_location -x)
        y (.. current-node -grid_location -y)]
    (set! (.-up_left ctx) true)
    (set! (.-up_right ctx) true)
    (set! (.-down_left ctx) true)
    (set! (.-down_right ctx) true)
    (set! (.-status ctx) 0)
    (set! (.-total_cost ctx) 0)

    (if (and (> x 0) (not (wall? ctx (dec x) y)))
      (do
        (set! (.-grid_location ctx) #js {:x (dec x) :y y})
        (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx)))
        (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
        (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                             (< (.. ctx -total_cost)
                                                (hs-get (.-node_costs ctx)
                                                        (.. ctx -grid_location)))))
          (add-node-to-open-list ctx (new-path-node current-node end_node
                                                    (.-grid_location ctx)
                                                    (+ cost-straight (.-direct_cost current-node))
                                                    (.-total_cost ctx)))))
      (do
        (set! (.-up_left ctx) false)
        (set! (.-down_left ctx) false)))

    #_(println (hs-get (.-node_costs ctx) (.. ctx -grid_location)) (.-status ctx))

    (if (and (< x 49) (not (wall? ctx (inc x) y)))
      (do
        (set! (.-grid_location ctx) #js {:x (inc x) :y y})
        (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx)))
        (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
        (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                             (< (.. ctx -total_cost)
                                                (hs-get (.-node_costs ctx)
                                                        (.. ctx -grid_location)))))
          (add-node-to-open-list ctx (new-path-node current-node end_node
                                                    (.-grid_location ctx)
                                                    (+ cost-straight (.-direct_cost current-node))
                                                    (.-total_cost ctx)))))
      (do
        (set! (.-up_right ctx) false)
        (set! (.-down_right ctx) false)))

    #_(println (hs-get (.-node_costs ctx) (.. ctx -grid_location)) (.-status ctx))

    (if (and (> y 0) (not (wall? ctx x (dec y))))
      (do
        (set! (.-grid_location ctx) #js {:x x :y (dec y)})
        (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx)))
        (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
        (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                             (< (.. ctx -total_cost)
                                                (hs-get (.-node_costs ctx)
                                                        (.. ctx -grid_location)))))
          (add-node-to-open-list ctx (new-path-node current-node end_node
                                                    (.-grid_location ctx)
                                                    (+ cost-straight (.-direct_cost current-node))
                                                    (.-total_cost ctx)))))
      (do
        (set! (.-up_left ctx) false)
        (set! (.-up_right ctx) false)))

    #_(println (hs-get (.-node_costs ctx) (.. ctx -grid_location)) (.-status ctx))

    (if (and (< y 49) (not (wall? ctx x (inc y))))
      (do
        (set! (.-grid_location ctx) #js {:x x :y (inc y)})
        (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx)))
        (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
        (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                             (< (.. ctx -total_cost)
                                                (hs-get (.-node_costs ctx)
                                                        (.. ctx -grid_location)))))
          (add-node-to-open-list ctx (new-path-node current-node end_node
                                                    (.-grid_location ctx)
                                                    (+ cost-straight (.-direct_cost current-node))
                                                    (.-total_cost ctx)))))
      (do
        (set! (.-down_left ctx) false)
        (set! (.-down_right ctx) false)))

    #_(println (hs-get (.-node_costs ctx) (.. ctx -grid_location)) (.-status ctx))

    (when (and (.-up_left ctx) (not (wall? ctx (dec x) (dec y))))
      (set! (.-grid_location ctx) #js {:x (dec x) :y (dec y)})
      (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx)))
      (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
      (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                           (< (.. ctx -total_cost)
                                              (hs-get (.-node_costs ctx)
                                                      (.. ctx -grid_location)))))
        (add-node-to-open-list ctx (new-path-node current-node end_node
                                                  (.-grid_location ctx)
                                                  (+ cost-diagonal (.-direct_cost current-node))
                                                  (.-total_cost ctx)))))

    #_(println (hs-get (.-node_costs ctx) (.. ctx -grid_location)) (.-status ctx))

    (when (and (.-up_right ctx) (not (wall? ctx (inc x) (dec y))))
      (set! (.-grid_location ctx) #js {:x (inc x) :y (dec y)})
      (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx)))
      (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
      (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                           (< (.. ctx -total_cost)
                                              (hs-get (.-node_costs ctx)
                                                      (.. ctx -grid_location)))))
        (add-node-to-open-list ctx (new-path-node current-node end_node
                                                  (.-grid_location ctx)
                                                  (+ cost-diagonal (.-direct_cost current-node))
                                                  (.-total_cost ctx)))))

    #_(println (hs-get (.-node_costs ctx) (.. ctx -grid_location)) (.-status ctx))

    (when (and (.-down_left ctx) (not (wall? ctx (dec x) (inc y))))
      (set! (.-grid_location ctx) #js {:x (dec x) :y (inc y)})
      (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx)))
      (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
      (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                           (< (.. ctx -total_cost)
                                              (hs-get (.-node_costs ctx)
                                                      (.. ctx -grid_location)))))
        (add-node-to-open-list ctx (new-path-node current-node end_node
                                                  (.-grid_location ctx)
                                                  (+ cost-diagonal (.-direct_cost current-node))
                                                  (.-total_cost ctx)))))

    #_(println (hs-get (.-node_costs ctx) (.. ctx -grid_location)) (.-status ctx))

    (when (and (.-down_right ctx) (not (wall? ctx (inc x) (inc y))))
      (set! (.-grid_location ctx) #js {:x (inc x) :y (inc y)})
      (set! (.-total_cost ctx) (total-cost end_node (.-grid_location ctx)))
      (set! (.-status ctx) (hs-get (.-node_status ctx) (.. ctx -grid_location)))
      (when (or (nil? (.-status ctx)) (and (= (.-status ctx) 1)
                                           (< (.. ctx -total_cost)
                                              (hs-get (.-node_costs ctx)
                                                      (.. ctx -grid_location)))))
        (add-node-to-open-list ctx (new-path-node current-node end_node
                                                  (.-grid_location ctx)
                                                  (+ cost-diagonal (.-direct_cost current-node))
                                                  (.-total_cost ctx)))))

    #_(println (hs-get (.-node_costs ctx) (.. ctx -grid_location)) (.-status ctx))))
(def tmp (atom 0))
(defn find-path [start-tile end-tile]
  (let [tiles (:map-squares @s/context)
        ctx   #js {:tiles tiles}]
    (when (not (or (wall? ctx (:x start-tile) (:y start-tile))
                   (wall? ctx (:x end-tile)   (:y end-tile))))
      (let [end_node (new-path-node nil nil #js {:x (:x end-tile) :y (:y end-tile)} 0 0)
            grid_location #js {:x (:x start-tile) :y (:y start-tile)}
            total_cost (total-cost end_node grid_location)
            start-node (new-path-node nil end_node grid_location 0 total_cost)]
        (set! (.-open_list ctx) nil)
        (set! (.-node_costs ctx) (array))
        (set! (.-node_status ctx) (array))
        (add-node-to-open-list ctx start-node)
        (loop []
          #_(swap! tmp inc)
          (when (and (.. ctx -open_list) #_(< @tmp 50))
            #_(println @tmp)
            (let [current-node (.. ctx -open_list)
                  ;; cnt (atom 0)
                  ]
              (set! (.-open_list ctx) (.-next current-node))
              (set! (.-next current-node) nil)

              (if (eq? current-node end_node)
                (loop [res nil
                       cn current-node]
                  #_(println (.-grid_location cn) (swap! cnt inc))
                  (if (.-parent_node cn)
                    (recur (.-grid_location cn) (.-parent_node cn))
                    {:x (.. res -x) :y (.. res -y)}))
                (do
                  (hs-set (.-node_costs ctx) (.-grid_location current-node) nil)
                  (find-adjacent-nodes ctx current-node end_node)
                  (hs-set (.-node_status ctx) (.-grid_location current-node) 0)
                  (recur))))))))))

#_(defn find-path [start-tile end-tile]
  (let [tiles (:map-squares @s/context)
        ctx   #js {:tiles tiles}]
    (when (not (or (wall? ctx (:x start-tile) (:y start-tile))
                   (wall? ctx (:x end-tile)   (:y end-tile))))
      (let [end_node (new-path-node nil nil #js {:x (:x end-tile) :y (:y end-tile)} 0 0)
            grid_location #js {:x (:x start-tile) :y (:y start-tile)}
            total_cost (total-cost end_node grid_location)
            start-node (new-path-node nil end_node grid_location 0 total_cost)]
        (set! (.-open_list ctx) (array))
        (set! (.-node_costs ctx) (array))
        (set! (.-node_status ctx) (array))
        (loop [ctx (add-node-to-open-list ctx start-node)]
          ;; (swap! tmp inc)
          (when (and (> (.. ctx -open_list -length) 0) (< @tmp 300))
            ;;(println @tmp)
            (let [current-node (.. ctx -open_list pop)
                  ;; cnt (atom 0)
                  ]
              (if (eq? current-node end_node)
                (loop [res nil
                       cn current-node]
                  #_(println (.-grid_location cn) (swap! cnt inc))
                  (if (.-parent_node cn)
                    (recur (.-grid_location cn) (.-parent_node cn))
                    {:x (.. res -x) :y (.. res -y)}))
                (do
                  (hs-set (.-node_costs ctx) (.-grid_location current-node) nil)
                  (find-adjacent-nodes ctx current-node end_node)
                  (hs-set (.-node_status ctx) (.-grid_location current-node) 0)
                  (recur ctx))))))))))
