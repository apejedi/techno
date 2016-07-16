(ns techno.core
  (:use [overtone.core]
        [techno.sequencer :as s]
        )
  )

(declare player)
(comment
  (if (or (nil? player) (not (node-active? player)))
      (def player (s/get-s
                   (/ 80 60)
                   )))
  (s/set-size player 5)
  (s/set-sp player 0.1)
  (s/set-sp player (/ 70 60))
  (s/set-st player (double (/ 1 8)
                    ))
  (kill player)
  )
