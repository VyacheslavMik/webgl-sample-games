(ns games.core
  (:require [games.engine :as engine]
            [games.flood-control.core         :as flood-control]
            [games.asteroid-belt-assault.core :as asteroid-belt-assault]
            [games.robot-rampage.core         :as robot-rampage]
            [games.gemstone-hunter.core       :as gemstone-hunter]
            [games.gemstone-hunter.editor]))

(enable-console-print!)

(defn ^:export set-hash [hash]
  (set! js/window.location.hash hash)
  (.reload js/window.location))

(defn help-content [hash]
  (case hash
    "#asteroid-belt-assault"
    "Controls:
W - move to the top
S - move to the bottom
A - move to the left
D - move to the right
Space - shot"

    "#robot-rampage"
    "Controls:
W - move to the top
S - move to the bottom
A - move to the left
D - move to the right
Mouse left or right click - shot"

    "#gemstone-hunter"
    "Controls:
W - enter to the door
A - move to the left
D - move to the right
Space - jump"

    "Controls:
Mouse left button click  - rotate clockwise
Mouse right button click - rotate counterclockwise"))

(defn activate-game [hash init]
  (let [el (.querySelector js/document hash)
        help (.querySelector js/document "#help")]
    (when el
      (set! (.. help -textContent) (help-content hash))
      (.. el -classList (add "active-game"))
      (init))))

(let [h js/window.location.hash]
  (case h
    "#asteroid-belt-assault"
    (activate-game h asteroid-belt-assault/init)

    "#robot-rampage"
    (activate-game h robot-rampage/init)

    "#gemstone-hunter"
    (activate-game h gemstone-hunter/init)

    (activate-game "#flood-control" flood-control/init)))

(defn ^:export start []  (engine/run))
  
