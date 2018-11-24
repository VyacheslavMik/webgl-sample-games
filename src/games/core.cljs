(ns games.core
  (:require [games.engine :as engine]
            [games.flood-control.core :as flood-control]
            [games.asteroid-belt-assault.core :as asteroid-belt-assault]))

(enable-console-print!)

(asteroid-belt-assault/init)

(defn ^:export start []  (engine/run))
