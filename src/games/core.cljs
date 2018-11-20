(ns games.core
  (:require [games.flood-control :as flood-control]))

(enable-console-print!)

(flood-control/run)
