(ns techno.core
  (:use [overtone.core]
        [techno.sequencer :as s]
        )
  )
(defonce s-player (atom nil))
(declare player)
(comment
  (if (or (nil? player) (not (node-active? player)))
      (def player (s/get-s
                   (/ 80 60)
                   )))
  (s/set-size player 5)
  (s/set-sp player 0.1)
  (s/set-sp player (/ 120 60))
  (s/set-st player (double (/ 1 4)
                    ))
  (kill player)
  )

(defn start-player []
  (let [player @s-player]
      (if (or (nil? player) (not (node-active? player)))
        (swap!
         s-player
         (fn [_]
           (s/get-s
            (/ 80 60))))))
  )
