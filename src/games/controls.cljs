(ns games.controls)

(def context (atom {:mouse-state  nil
                    :touch-state  nil
                    :pressed-keys #{}}))

(defn handle-key-down [ev]
  (swap! context update :pressed-keys conj (keyword (.-code ev))))

(defn handle-key-up [ev]
  (swap! context update :pressed-keys disj (keyword (.-code ev))))

(defn handle-context-menu [ev]
  (.preventDefault ev))

(defn handle-mouse-down [ev]
  (let [button ({0 :left 2 :right} (.-button ev))]
    (swap! context assoc :mouse-state {:button button
                                       :x (.-offsetX ev)
                                       :y (.-offsetY ev)})))

(defn handle-mouse-up [ev]
  (swap! context assoc :mouse-state nil))

(defn handle-mouse-move [ev]
  (when-let [button ({1 :left 2 :right 3 :left} (.-buttons ev))]
    (swap! context assoc :mouse-state {:button button
                                       :x (.-offsetX ev)
                                       :y (.-offsetY ev)})))

(defn handle-touch-start [ev]
  (let [rect (.getBoundingClientRect (:text-canvas @context))
        touches (mapv (fn [i]
                        (let [touch (.. ev -touches (item i))]
                          {:x (- (.-clientX touch) (.-left rect))
                           :y (- (.-clientY touch) (.-top rect))}))
                      (range (.. ev -touches -length)))]
    (swap! context assoc :touch-state touches)))

 (defn handle-touch-end [ev]
  (swap! context assoc :touch-state nil))

(defn key-pressed? [k]
  (get (:pressed-keys @context) k))

(defn get-mouse-state []
  (:mouse-state @context))

(defn get-touch-state []
  (:touch-state @context))

(defn register-events [renderer]
  (.removeEventListener renderer         "contextmenu" handle-context-menu)
  (.removeEventListener js/document.body "keydown"     handle-key-down)
  (.removeEventListener js/document.body "keyup"       handle-key-up)
  (.removeEventListener renderer         "mousedown"   handle-mouse-down)
  (.removeEventListener renderer         "mouseup"     handle-mouse-up)
  (.removeEventListener renderer         "mousemove"   handle-mouse-move)
  (.removeEventListener renderer         "touchstart"  handle-touch-start)
  (.removeEventListener renderer         "touchend"    handle-touch-end)

  (.addEventListener    renderer         "contextmenu" handle-context-menu)
  (.addEventListener    js/document.body "keydown"     handle-key-down)
  (.addEventListener    js/document.body "keyup"       handle-key-up)
  (.addEventListener    renderer         "mousedown"   handle-mouse-down)
  (.addEventListener    renderer         "mouseup"     handle-mouse-up)
  (.addEventListener    renderer         "mousemove"   handle-mouse-move)
  (.addEventListener    renderer         "touchstart"  handle-touch-start)
  (.addEventListener    renderer         "touchend"    handle-touch-end))
