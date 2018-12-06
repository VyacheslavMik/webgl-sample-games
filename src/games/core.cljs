(ns games.core
  (:require [games.engine :as engine]
            [games.flood-control.core         :as flood-control]
            [games.asteroid-belt-assault.core :as asteroid-belt-assault]
            [games.robot-rampage.core         :as robot-rampage]
            [games.gemstone-hunter.editor]))

(enable-console-print!)

;;(robot-rampage/init)

(defn ^:export start []  (engine/run))
