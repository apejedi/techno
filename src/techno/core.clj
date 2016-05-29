(ns techno.core
  (:use [overtone.core]
        [techno.sequencer :as s]
        )
  )

(declare player)
(comment
  (if (or (nil? player) (not (node-active? player)))
      (def player (s/get-s
                                        ;2
                   (/ 80 60)
                                        ;0.25
                   (double (/ 1 4))
                   )))
  (s/set-sp player 0.1)
  (s/set-sp player (/ 40 60))
  (s/set-st player (double (/ 1 4)
                    ))
  (kill player)
  )
