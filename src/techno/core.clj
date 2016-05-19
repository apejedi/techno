(ns techno.core
  (:use [overtone.core]
        [techno.sequencer :as s]
        )
  )


(comment
  (def player (s/get-s
               ;2
               (/ 80 60)
               0.25))
  (kill player)
  )
