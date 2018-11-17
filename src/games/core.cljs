(ns games.core)

(def cube-rotation (atom 0.0))
(def then            (atom 0))
(def delta-time      (atom 0))

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
    }")

(def my-color (js/Float32Array. [1.0 1.0 1.0 1.0]))
(def color-white (js/Float32Array. [1.0 1.0 1.0 1.0]))

(defn is-power-of-2 [value]
  (= (bit-and value (dec value)) 0))

(defn load-texture [gl url]
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
    texture))

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

(defn draw-rectangle [{buffers :buffers
                       gl :gl
                       texture :texture
                       program-info :program-info
                       color :color
                       effect :effect
                       origin :origin
                       size :size}]
  (let [half-height (/ (:height size) 2)
        half-width (/ (:width size) 2)
        positions [   half-width     half-height
                   (- half-width)    half-height
                      half-width  (- half-height)
                   (- half-width) (- half-height)]
        texture-coordinates [1.0  1.0
                             0.0  1.0
                             1.0  0.0
                             0.0  0.0]
        model-view-matrix (.create js/mat4)]

    (.translate js/mat4 model-view-matrix model-view-matrix #js[(:x origin) (:y origin) 0.0])

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
      (.drawArrays gl (.-TRIANGLE_STRIP gl) offset vertex-count))))

(defn draw-text [text]
  (let [canvas      (.querySelector js/document "#textCanvas")
        ctx         (.getContext canvas "2d")
        width       (.-width (.measureText ctx text))]
    (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
    (set! (.-font ctx) "20px Verdana")
    (set! (.-fillStyle ctx) "white")
    (set! (.-textAlign ctx) "center")
    (set! (.-textBaseline ctx) "middle")
    (.fillText ctx text 500 40)))

(defn init-buffers [gl]
  (let [position-buffer (.createBuffer gl)
        texture-coord-buffer (.createBuffer gl)]
    {:position      position-buffer
     :texture-coord texture-coord-buffer}))

(defn draw-scene [gl program-info buffers texture]
  (.clearColor gl 0.0 0.0 0.0 1.0)
  (.clearDepth gl 1.0)
  (.enable     gl (.-DEPTH_TEST gl))
  (.depthFunc  gl (.-LEQUAL gl))

  (.clear gl (bit-or (.-COLOR_BUFFER_BIT gl) (.-DEPTH_BUFFER_BIT gl)))

  (let [field-of-view (* 30 (/ Math/PI 180))
        aspect (/ (.. gl -canvas -clientWidth)
                  (.. gl -canvas -clientHeight))
        z-near 0.0
        z-far 100.0
        projection-matrix (.create js/mat4)]
    (.ortho     js/mat4 projection-matrix 0 (.. gl -canvas -clientWidth) (.. gl -canvas -clientHeight) 0 0 1)
    (.useProgram gl (:program program-info))
    (.uniformMatrix4fv gl
                       (-> program-info :uniform-locations :projection-matrix)
                       false
                       projection-matrix)

    (draw-rectangle {:buffers buffers
                     :gl gl
                     :texture texture
                     :program-info program-info
                     :size {:width 60 :height 60}
                     :origin {:x 40 :y 40}
                     :color my-color})

    (draw-rectangle {:buffers buffers
                     :gl gl
                     :texture texture
                     :program-info program-info
                     :size {:width 60 :height 60}
                     :origin {:x 110 :y 40}
                     :color my-color
                     :effect {:type :rotate
                              :angle 45}})

    (draw-rectangle {:buffers buffers
                     :gl gl
                     :texture texture
                     :program-info program-info
                     :size {:width 60 :height 60}
                     :origin {:x 180 :y 40}
                     :effect {:type :flip}
                     :color my-color})

    (draw-text "Some text")
))

(def context  (atom nil))

(defn main []
  (let [canvas (.querySelector js/document "#glCanvas")
        gl     (.getContext canvas "webgl")]
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
            texture (load-texture gl "cubetexture.png")]
        (reset! context {:canvas       canvas
                         :gl           gl
                         :texture      texture
                         :program-info program-info
                         :buffers      buffers})
        (draw-scene gl program-info buffers texture)))))

(main)

(defn render [now]
  (when-let [{:keys [gl program-info buffers texture]} @context]
    (let [now (* now 0.001)]
      (reset! delta-time (- now @then))
      (reset! then now))
    (draw-scene gl program-info buffers texture)
    (js/requestAnimationFrame render)))

(js/requestAnimationFrame render)
