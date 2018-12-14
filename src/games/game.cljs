(ns games.game
  (:require [games.controls :as controls]))

(defn run [app update* root]
  (.. app -stage (addChild root))
  (controls/register-events (.. app -view))
  (.. app -ticker (add update*)))
