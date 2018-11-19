(ns games.engine)

(def context         (atom nil))
(def then            (atom 0))
(def delta-time      (atom 0))

(def color-white (js/Float32Array. [1.0 1.0 1.0 1.0]))

(defn color [v]
  (js/Float32Array. v))

(defn tex-coords [tex-width tex-height {:keys [x y w h]}]
  (let []
    ))

(def vs-source "
    attribute vec4 aVertexPosition;
    attribute vec2 aTextureCoord;

    uniform mat4 uModelViewMatrix;
    uniform mat4 uProjectionMatrix;

    varying highp vec2 vTextureCoord;

    void main(void) {
      gl_Position = uProjectionMatrix * uModelViewMatrix * aVertexPosition;
      vTextureCoord = aTextureCoord;
    }")

(def fs-source "
    precision mediump float;

    varying highp vec2 vTextureCoord;

    uniform sampler2D uSampler;
    uniform vec4 uColor;

    void main() {
      gl_FragColor = texture2D(uSampler, vTextureCoord) * uColor;
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

(defn draw-rectangle [{:keys [color effect origin position size texture tex-coords]}]
  (when-let [{:keys [buffers gl program-info textures]} @context]
    (let [tex-size (get textures texture)
          size (or size
                   (and tex-coords {:width (:w tex-coords)
                                    :height (:h tex-coords)})
                   tex-size)
          half-height (/ (:height size) 2)
          half-width  (/ (:width size) 2)
          positions [   half-width     half-height
                     (- half-width)    half-height
                     half-width  (- half-height)
                     (- half-width) (- half-height)]
          texture-coordinates (if (and tex-coords tex-size)
                                (let [{:keys [x y w h]} tex-coords
                                      {:keys [width height]} tex-size
                                      x2 (/ (+ x w) width)
                                      y2 (/ (+ y h) height)
                                      x1 (/ x width)
                                      y1 (/ y height)]
                                  [x2 y2
                                   x1 y2
                                   x2 y1
                                   x1 y1])
                                [1.0  1.0
                                 0.0  1.0
                                 1.0  0.0
                                 0.0  0.0])
          model-view-matrix (.create js/mat4)]

      (let [[x y] (cond
                    origin
                    [(:x origin) (:y origin)]

                    position
                    [(+ (:x position) half-width) (+ (:y position) half-height)]

                    :else
                    [half-width half-height])]
        (.translate js/mat4 model-view-matrix model-view-matrix #js[x y 0.0]))

      (case (:type effect)
        :rotate
        (.rotate js/mat4 model-view-matrix model-view-matrix (* (:angle effect) (/ Math/PI 180)) #js[0 0 1])

        :flip
        (.rotate js/mat4 model-view-matrix model-view-matrix (* 180 (/ Math/PI 180)) #js[0 1 0])

        nil)

      (.uniformMatrix4fv gl
                         (-> program-info :uniform-locations :model-view-matrix)
                         false
                         model-view-matrix)

      (.bindBuffer gl (.-ARRAY_BUFFER gl) (:position buffers))
      (.bufferData gl (.-ARRAY_BUFFER gl) (js/Float32Array. positions) (.-STATIC_DRAW gl))

      (.bindBuffer gl (.-ARRAY_BUFFER gl) (:texture-coord buffers))
      (.bufferData gl (.-ARRAY_BUFFER gl) (js/Float32Array. texture-coordinates) (.-STATIC_DRAW gl))

      (let [num-components 2
            type (.-FLOAT gl)
            normalize false
            stride 0
            offset 0]
        (.bindBuffer gl (.-ARRAY_BUFFER gl) (:position buffers))
        (.vertexAttribPointer gl
                              (-> program-info :attrib-locations :vertex-position)
                              num-components
                              type
                              normalize
                              stride
                              offset)
        (.enableVertexAttribArray gl (-> program-info :attrib-locations :vertex-position)))

      (let [num-components 2
            type (.-FLOAT gl)
            normalize false
            stride 0
            offset 0]
        (.bindBuffer gl (.-ARRAY_BUFFER gl) (:texture-coord buffers))
        (.vertexAttribPointer gl
                              (-> program-info :attrib-locations :texture-coord)
                              num-components
                              type
                              normalize
                              stride
                              offset)
        (.enableVertexAttribArray gl (-> program-info :attrib-locations :texture-coord)))

      (if color
        (.uniform4fv gl (-> program-info :uniform-locations :u-color) color)
        (.uniform4fv gl (-> program-info :uniform-locations :u-color) color-white))

      (.activeTexture gl (.-TEXTURE0 gl))
      (.bindTexture gl (.-TEXTURE_2D gl) texture)
      (.uniform1i gl (-> program-info :uniform-locations :u-sampler) 0)
      
      (let [offset 0
            vertex-count 4]
        (.drawArrays gl (.-TRIANGLE_STRIP gl) offset vertex-count)))))

(defn rgba-color [color]
  (str "rgba(" (Math/floor (* 255 (aget color 0)))
       ","     (Math/floor (* 255 (aget color 1)))
       ","     (Math/floor (* 255 (aget color 2)))
       ","     (aget color 3)
       ")"))

(defn draw-text [{:keys [text origin color scale font]}]
  (let [{ctx :text-context canvas :text-canvas font* :font} @context
        {:keys [family size]} (merge font* font)
        width  (.-width (.measureText ctx text))
        height (if scale (Math/floor (* size scale)) size)]
    (set! (.-font ctx)        (str height "px " family))
    (set! (.-fillStyle ctx)   (if color (rgba-color color) "white"))
    (set! (.-textAlign ctx)    "center")
    (set! (.-textBaseline ctx) "middle")
    (.fillText ctx text (:x origin) (:y origin))))

(defn init-buffers [gl]
  (let [position-buffer (.createBuffer gl)
        texture-coord-buffer (.createBuffer gl)]
    {:position      position-buffer
     :texture-coord texture-coord-buffer}))

(defn draw []
  (when-let [{:keys [gl program-info buffers texture projection-matrix draw-fn] :as ctx} @context]
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
      (draw-fn))))

(defn handle-mouse-click [ev]
  (.log js/console "click" ev)
  )

(defn handle-key-down [ev]
  (swap! context update :pressed-keys conj (keyword (.-code ev))))

(defn handle-key-up [ev]
  (swap! context update :pressed-keys disj (keyword (.-code ev))))

(defn handle-context-menu [ev]
  (.log js/console "click" ev)
  (.preventDefault ev))

(defn key-pressed? [k]
  (get (:pressed-keys @context) k))

(defn register-events [text-canvas]
  (.removeEventListener text-canvas "contextmenu" handle-context-menu)
  (.removeEventListener text-canvas "click"       handle-mouse-click)
  (.removeEventListener text-canvas "keydown"     handle-key-down)
  (.removeEventListener text-canvas "keyup"       handle-key-up)

  (.addEventListener    text-canvas "contextmenu" handle-context-menu)
  (.addEventListener    text-canvas "click"       handle-mouse-click)
  (.addEventListener    text-canvas "keydown"     handle-key-down)
  (.addEventListener    text-canvas "keyup"       handle-key-up))

(defn init [& [props]]
  (let [gl-canvas    (.querySelector js/document "#glCanvas")
        gl           (.getContext gl-canvas "webgl" #js{:premultipliedAlpha false
                                                        :alpha              false})
        text-canvas  (.querySelector js/document "#textCanvas")
        text-context (.getContext text-canvas "2d")]
    (if (nil? gl)
      (js/alert "Unable to initialize WebGL. Your browser or machine may not support it.")
      (let [program (init-shader-program gl vs-source fs-source)
            vertex-position   (.getAttribLocation  gl program "aVertexPosition")
            texture-coord     (.getAttribLocation  gl program "aTextureCoord")
            projection-matrix (.getUniformLocation gl program "uProjectionMatrix")
            model-view-matrix (.getUniformLocation gl program "uModelViewMatrix")
            u-sampler         (.getUniformLocation gl program "uSampler")
            u-color           (.getUniformLocation gl program "uColor")
            program-info {:program program
                          :attrib-locations {:vertex-position vertex-position
                                             :texture-coord   texture-coord}
                          :uniform-locations {:projection-matrix projection-matrix
                                              :model-view-matrix model-view-matrix
                                              :u-sampler         u-sampler
                                              :u-color           u-color}}
            buffers (init-buffers gl)
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
                         :projection-matrix projection-matrix
                         :program-info      program-info
                         :buffers           buffers
                         :pressed-keys      #{}
                         :update-fn         (:update-fn props)
                         :draw-fn           (:draw-fn props)})))))

(defn render [now]
  (when-let [{:keys [update-fn]} @context]
      (reset! delta-time (- now @then))
      (reset! then now)
    (when update-fn
      (update-fn))
    (draw)
    (js/requestAnimationFrame render)))

(defn run []
  (js/requestAnimationFrame render))
