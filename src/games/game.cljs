(ns games.game
  (:require [games.controls :as controls]))

(defn run [app update* root]
  (.. app -stage (addChild root))
  (controls/register-events (.. app -view))
  (.. app -ticker (add (fn [_]
                         (update* (.. app -ticker -elapsedMS))))))

(defn play-sound [url]
  (.play (js/Audio. url)))
