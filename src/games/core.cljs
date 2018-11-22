(ns games.core
  (:require [games.flood-control.core :as flood-control]))

(enable-console-print!)

(flood-control/run)
