(ns games.engine)

(defonce context         (atom nil))
(def     then            (atom 0))

(def color-white #js {:r 1.0 :g 1.0 :b 1.0 :a 1.0})
(defn color [[r g b a]] #js {:r r :g g :b b :a a})
(defn rgb-color [[r g b a]]
  #js {:r (/ r 255) :g (/ g 255) :b (/ b 255) :a (/ a 255)})

(defn play-sound [url]
  (.play (js/Audio. url)))

(def vs-source "
    attribute vec4 aVertexPosition;
    attribute vec4 aVertexColor;
    attribute vec2 aTextureCoord;

    uniform mat4 uProjectionMatrix;

    varying highp vec2 vTextureCoord;
    varying lowp vec4 vColor;

    void main(void) {
      gl_Position = uProjectionMatrix * aVertexPosition;
      vTextureCoord = aTextureCoord;
      vColor = aVertexColor;
    }")

(def fs-source "
    precision mediump float;

    varying highp vec2 vTextureCoord;
    varying lowp vec4 vColor;

    uniform sampler2D uSampler;

    void main() {
      gl_FragColor = texture2D(uSampler, vTextureCoord) * vColor;
      gl_FragColor.rgb *= gl_FragColor.a;
    }")

(defn is-power-of-2 [value]
  (= (bit-and value (dec value)) 0))

(defn load-texture [url]
  (when-let [{:keys [gl]} @context]
    (let [texture (.createTexture gl)]
      (.bindTexture gl (.-TEXTURE_2D gl) texture)

      (let [level 0
            internal-format (.-RGBA gl)
            width 1
            height 1
            border 0
            src-format (.-RGBA gl)
            src-type (.-UNSIGNED_BYTE gl)
            pixel (js/Uint8Array. #js[0 0 255 255])
            image (js/Image.)]
        (.texImage2D gl (.-TEXTURE_2D gl) level internal-format width
                     height border src-format src-type pixel)
        (set! (.-onload image)
              (fn []
                (swap! context assoc-in [:textures texture] {:width (.-width image)
                                                             :height (.-height image)})
                (.bindTexture gl (.-TEXTURE_2D gl) texture)
                (.texImage2D gl (.-TEXTURE_2D gl) level internal-format
                             src-format src-type image)
                (if (and (is-power-of-2 (.-width image))
                         (is-power-of-2 (.-height image)))
                  (.generateMipmap gl (.-TEXTURE_2D gl))
                  (do
                    (.texParameteri gl (.-TEXTURE_2D gl) (.-TEXTURE_WRAP_S gl) (.-CLAMP_TO_EDGE gl))
                    (.texParameteri gl (.-TEXTURE_2D gl) (.-TEXTURE_WRAP_T gl) (.-CLAMP_TO_EDGE gl))
                    (.texParameteri gl (.-TEXTURE_2D gl) (.-TEXTURE_MIN_FILTER gl) (.-LINEAR gl))))))
        (set! (.-src image) url))
      texture)))

(defn load-shader [gl type source]
  (let [shader (.createShader gl type)]
    (.shaderSource gl shader source)
    (.compileShader gl shader)
    (if (.getShaderParameter gl shader (.-COMPILE_STATUS gl))
      shader
      (do
        (js/alert
         (str "An error occurred compiling the shaders: "
              (.getShaderInfoLog gl shader)))
        nil))))

(defn init-shader-program [gl vs-source fs-source]
  (let [vertex-shader   (load-shader gl (.-VERTEX_SHADER gl) vs-source)
        fragment-shader (load-shader gl (.-FRAGMENT_SHADER gl) fs-source)
        shader-program  (.createProgram gl)]
    (.attachShader gl shader-program vertex-shader)
    (.attachShader gl shader-program fragment-shader)
    (.linkProgram  gl shader-program)

    (if (.getProgramParameter gl shader-program (.-LINK_STATUS gl))
      shader-program
      (do
        (js/alert
         (str "Unable to initialize the shader program: "
              (.getProgramInfoLog gl shader-program)))
        nil))))

(defn rgba-color [color]
  (str "rgba(" (Math/floor (* 255 (.-r color)))
       ","     (Math/floor (* 255 (.-g color)))
       ","     (Math/floor (* 255 (.-b color)))
       ","     (.-a color)
       ")"))

(defn draw-text [{:keys [text color scale font position align]}]
  (let [{ctx :text-context canvas :text-canvas font* :font} @context
        {:keys [family size]} (merge font* font)
        width  (.-width (.measureText ctx text))
        height (if scale (Math/floor (* size scale)) size)
        {:keys [x y]} (or position {:x 0 :y 0})]
    (set! (.-font ctx)        (str height "px " family))
    (set! (.-fillStyle ctx)   (if color (rgba-color color) "white"))
    (set! (.-textAlign ctx) (or (and align (name align)) "center"))
    (set! (.-textBaseline ctx) "middle")
    (.fillText ctx text x y)))

(defn rotate-point [arr x y cos-angle sin-angle dx dy]
  (let [x' (- (* x cos-angle) (* y sin-angle))
        y' (+ (* x sin-angle) (* y cos-angle))]
    (.push arr (+ x' dx) (+ y' dy))))

(defn add-vertex [arr rotate? x y cos sin dx dy]
  (if rotate?
    (rotate-point arr x y cos sin dx dy)
    (.push arr (+ x dx) (+ y dy))))

(defn prepare-rectangle [item rectangle]
  (let [data (.-data item)
        {:keys [color effect origin position size texture tex-coords]} rectangle
        tex-size (.-texsize item)
        size (or size
                 (and tex-coords {:width (:w tex-coords)
                                  :height (:h tex-coords)})
                 tex-size)
        half-height (/ (:height size) 2)
        half-width  (/ (:width size) 2)

        vx1 (- half-width)  vx2 half-width
        vy1 (- half-height) vy2 half-height

        rotate? (= (:type effect) :rotate)
        angle   (:angle effect)
        radians (:radians effect)

        angle-rad (when rotate? (if angle (* angle (/ Math/PI 180)) radians))
        cos (when rotate? (Math/cos angle-rad))
        sin (when rotate? (Math/sin angle-rad))

        x (cond
            origin   (:x origin)
            position (+ (:x position) half-width)
            :else    half-width)

        y (cond
            origin   (:y origin)
            position (+ (:y position) half-height)
            :else    half-height)

        color (or color color-white)

        r (.-r color)
        g (.-g color)
        b (.-b color)
        a (.-a color)

        tex-coords (if (and tex-coords tex-size)
                     (let [{:keys [x y w h]} tex-coords
                           {:keys [width height]} tex-size
                           x2 (/ (+ x w) width)
                           y2 (/ (+ y h) height)
                           x1 (/ x width)
                           y1 (/ y height)]
                       #js {:tx1 x1 :tx2 x2 :ty1 y1 :ty2 y2})
                     #js {:tx1 0 :tx2 1 :ty1 0 :ty2 1})
        tx1 (.-tx1 tex-coords)
        tx2 (.-tx2 tex-coords)
        ty1 (.-ty1 tex-coords)
        ty2 (.-ty2 tex-coords)

        flip? (= (:type effect) :flip)]
    ;; v1
    (add-vertex data rotate? vx2 vy2 cos sin x y)
    (.push data r g b a)
    (if flip?
      (.push data tx1 ty2)
      (.push data tx2 ty2))

    ;; v2
    (add-vertex data rotate? vx1 vy2 cos sin x y)
    (.push data r g b a)
    (if flip?
      (.push data tx2 ty2)
      (.push data tx1 ty2))

    ;; v3
    (add-vertex data rotate? vx2 vy1 cos sin x y)
    (.push data r g b a)
    (if flip?
      (.push data tx1 ty1)
      (.push data tx2 ty1))

    ;; v4
    (add-vertex data rotate? vx1 vy2 cos sin x y)
    (.push data r g b a)
    (if flip?
      (.push data tx2 ty2)
      (.push data tx1 ty2))

    ;; v5
    (add-vertex data rotate? vx2 vy1 cos sin x y)
    (.push data r g b a)
    (if flip?
      (.push data tx1 ty1)
      (.push data tx2 ty1))

    ;; v6
    (add-vertex data rotate? vx1 vy1 cos sin x y)
    (.push data r g b a)
    (if flip?
      (.push data tx2 ty1)
      (.push data tx1 ty1))))

(def rectangles #js {:current  nil
                     :all      nil})

(defn new-rectangles []
  (set! (.-current rectangles) nil)
  (set! (.-all rectangles) (array)))

(defn get-rectangle [texture]
  (when (or (not (.-current rectangles))
            (not= texture (.. rectangles -current -texture)))
    (set! (.-current rectangles) #js {:texture texture
                                      :texsize (get-in @context [:textures texture])
                                      :data (array)})
    (.push (.-all rectangles) (.-current rectangles)))
  (.. rectangles -current))

(defn draw-rectangle [rectangle]
  (prepare-rectangle (get-rectangle (:texture rectangle)) rectangle))

(defn prepare-vertex-attrib [{:keys [gl program-info]} k num-components stride offset]
  (let [type (.-FLOAT gl)
        normalize false]
    (.vertexAttribPointer gl
                          (-> program-info :attrib-locations k)
                          num-components
                          type
                          normalize
                          stride
                          offset)
    (.enableVertexAttribArray gl (-> program-info :attrib-locations k))))

(defn prepare-buffer [{:keys [gl buffer] :as ctx} data]
  (.bindBuffer gl (.-ARRAY_BUFFER gl) buffer)
  (.bufferData gl (.-ARRAY_BUFFER gl) (js/Float32Array. data) (.-STATIC_DRAW gl))
  (prepare-vertex-attrib ctx :vertex-position    2 32 0)
  (prepare-vertex-attrib ctx :vertex-color       4 32 8)
  (prepare-vertex-attrib ctx :texture-coordinate 2 32 24))

(defn draw []
  (when-let [{:keys [gl program-info projection-matrix draw-fn] :as ctx} @context]
    (.clearColor gl 0.0 0.0 0.0 1.0)
    (.clearDepth gl 1.0)
    (.enable     gl (.-BLEND gl))

    (.blendFunc gl (.-SRC_ALPHA gl) (.-ONE_MINUS_SRC_ALPHA gl))

    (.clear gl (bit-or (.-COLOR_BUFFER_BIT gl) (.-DEPTH_BUFFER_BIT gl)))
    (.clearRect (:text-context ctx) 0 0 (.-width (:text-canvas ctx)) (.-height (:text-canvas ctx)))

    (.ortho     js/mat4 projection-matrix 0 (.. gl -canvas -clientWidth) (.. gl -canvas -clientHeight) 0 0 1)
    (.useProgram gl (:program program-info))
    (.uniformMatrix4fv gl
                       (-> program-info :uniform-locations :projection-matrix)
                       false
                       projection-matrix)
    (when draw-fn
      (draw-fn)
      (doseq [item (.-all rectangles)]
        (prepare-buffer ctx (.-data item))
        (.activeTexture gl (.-TEXTURE0 gl))
        (.bindTexture gl (.-TEXTURE_2D gl) (.-texture item))
        (.uniform1i gl (-> program-info :uniform-locations :u-sampler) 0)
        (.drawArrays gl (.-TRIANGLES gl) 0 (/ (count (.-data item)) 8))))))

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
        touch (.. ev -touches (item 0))]
    (swap! context assoc :touch-state {:x (- (.-clientX touch) (.-left rect))
                                       :y (- (.-clientY touch) (.-top rect))})))
 (defn handle-touch-end [ev]
  (swap! context assoc :touch-state nil))

(defn key-pressed? [k]
  (get (:pressed-keys @context) k))

(defn get-mouse-state []
  (:mouse-state @context))

(defn get-touch-state []
  (:touch-state @context))

(defn register-events [text-canvas]
  (.removeEventListener text-canvas "contextmenu" handle-context-menu)
  (.removeEventListener text-canvas "keydown"     handle-key-down)
  (.removeEventListener text-canvas "keyup"       handle-key-up)
  (.removeEventListener text-canvas "mousedown"   handle-mouse-down)
  (.removeEventListener text-canvas "mouseup"     handle-mouse-up)
  (.removeEventListener text-canvas "mousemove"   handle-mouse-move)
  (.removeEventListener text-canvas "touchstart"  handle-touch-start)
  (.removeEventListener text-canvas "touchend"    handle-touch-end)

  (.addEventListener    text-canvas "contextmenu" handle-context-menu)
  (.addEventListener    text-canvas "keydown"     handle-key-down)
  (.addEventListener    text-canvas "keyup"       handle-key-up)
  (.addEventListener    text-canvas "mousedown"   handle-mouse-down)
  (.addEventListener    text-canvas "mouseup"     handle-mouse-up)
  (.addEventListener    text-canvas "mousemove"   handle-mouse-move)
  (.addEventListener    text-canvas "touchstart"  handle-touch-start)
  (.addEventListener    text-canvas "touchend"    handle-touch-end))

(defn init [& [props]]
  (let [gl-canvas    (.querySelector js/document "#glCanvas")
        gl           (.getContext gl-canvas "webgl" #js{:premultipliedAlpha false
                                                        :alpha              false})
        text-canvas  (.querySelector js/document "#textCanvas")
        text-context (.getContext text-canvas "2d")
        fps-div      (when (:show-fps? props) (.querySelector js/document "#fps"))]
    (if (nil? gl)
      (js/alert "Unable to initialize WebGL. Your browser or machine may not support it.")
      (let [program (init-shader-program gl vs-source fs-source)
            vertex-position   (.getAttribLocation  gl program "aVertexPosition")
            vertex-color      (.getAttribLocation  gl program "aVertexColor")
            texture-coord     (.getAttribLocation  gl program "aTextureCoord")
            projection-matrix (.getUniformLocation gl program "uProjectionMatrix")
            u-sampler         (.getUniformLocation gl program "uSampler")
            program-info {:program program
                          :attrib-locations {:vertex-position    vertex-position
                                             :vertex-color       vertex-color
                                             :texture-coordinate texture-coord}
                          :uniform-locations {:projection-matrix projection-matrix
                                              :u-sampler         u-sampler}}
            projection-matrix (.create js/mat4)]
        (register-events text-canvas)
        (.focus text-canvas)
        (reset! context {:gl-canvas    gl-canvas
                         :gl           gl
                         :text-canvas  text-canvas
                         :text-context text-context
                         :font (or (:font props)
                                   {:family "Arial"
                                    :size 16})
                         :fps-div           fps-div
                         :projection-matrix projection-matrix
                         :program-info      program-info
                         :buffer            (.createBuffer gl)
                         :pressed-keys      #{}
                         :mouse-state       nil
                         :update-fn         (:update-fn props)
                         :draw-fn           (:draw-fn props)})))))

(defn show-fps [fps-div delta]
  (when fps-div
    (set! (.-textContent fps-div) (Math/floor (/ 1000 delta)))))

(def measure-time false)

(defn render [now]
  (when-let [{:keys [fps-div update-fn]} @context]
    (let [delta (- now @then)]
      (show-fps fps-div delta)
      (reset! then now)
      (if measure-time
        (do
          (print "Update ")
          (time
           (when update-fn (update-fn delta))))
        (when update-fn (update-fn delta)))
      (new-rectangles)
      (if measure-time
        (do
          (print "Draw ")
          (time (draw)))
        (draw))
      (js/requestAnimationFrame render))))

(defn run []
  (js/requestAnimationFrame render))
