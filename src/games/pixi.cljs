(ns games.pixi
  (:require [clojure.set :as clj-set]))

(defonce loader (js/PIXI.loaders.Loader.))
(defonce loaded-resources (atom #{}))

(defn init
  ([]
   (init [] #()))
  ([resources onload]
   (let [loading-resources (remove @loaded-resources resources)
         pixi-dom (.querySelector js/document "#pixi")
         app (js/PIXI.Application. #js{:width 800
                                       :height 600
                                       :backgroundColor 0x000000})]
     (when-not (zero? (.. pixi-dom -children -length))
       (.remove (aget (.. pixi-dom -children) 0)))
     (if (zero? (count loading-resources))
       (.appendChild pixi-dom (.. app -view))
       (do
         (.. loader (add (to-array loading-resources)))
         (.. loader (load (fn [loader resources]
                            (.appendChild pixi-dom (.. app -view))
                            (onload))))))
     (swap! loaded-resources clj-set/union (set resources))
     (.. app -renderer -plugins -interaction destroy)
     app)))

#_(let [pixi-dom (.querySelector js/document "#pixi")
      app (js/PIXI.Application. #js{:width 800
                                    :height 600
                                    :backgroundColor 0x000000})
      gem-base-texture (js/PIXI.BaseTexture.fromImage "textures/gemstone_hunter/Gem.png")
      gems (to-array
            (for [i (range 5)]
              (js/PIXI.Texture. gem-base-texture
                                (js/PIXI.Rectangle. (* i 48) 0 48 48)
                                (js/PIXI.Rectangle. (* i 48) 0 48 48))))
      anim (js/PIXI.extras.AnimatedSprite. gems)
      camera (js/PIXI.Container.)
      position (.. camera -position clone)
      tile #js {:sprite (js/PIXI.Sprite. (get tile-map/tile-textures 4))
                :index 4}
      text (js/PIXI.Text. "some text" #js{:fontFamily "Arial" :fontSize 10 :fill "red"})]
  (when-not (zero? (.. pixi-dom -children -length))
    (.remove (aget (.. pixi-dom -children) 0)))
  (.appendChild pixi-dom (.. app -view))
  (controls/register-events (.. app -view))

  (set! (.. anim -animationSpeed) 0.15)
  (.. anim play)

  (.. position (set 0 0))

  #_(println text)
  #_(.. text -position (set 100 100))

  #_(.. camera (addChild anim))
  #_(.. camera (addChild text))
  #_(.. camera (addChild (.. tile -sprite)))

  #_(.. tile -sprite -position (set 48 0))

  (.. app -stage (addChild camera/container))

  (.. app -ticker
      (add (fn [delta]
             (editor/update* delta))))
  )
