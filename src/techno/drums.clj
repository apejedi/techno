(ns techno.drums
  (:use [overtone.core]
        [overtone.inst.drum]
        [techno.samples]
        [techno.recorder]
        )
  (:require [techno.sequencer :as s])
  )


(def cymbals (create-sample-map "D:\\musicradar-drum-samples\\musicradar-drum-samples\\Assorted Hits\\Cymbals"))
(def hi-hats (create-sample-map "D:\\musicradar-drum-samples\\musicradar-drum-samples\\Assorted Hits\\Hi Hats"))
(def kicks (create-sample-map "D:\\musicradar-drum-samples\\musicradar-drum-samples\\Assorted Hits\\Kicks"))
(def snares (create-sample-map "D:\\musicradar-drum-samples\\musicradar-drum-samples\\Assorted Hits\\Snares"))
(def drum-kits (create-sample-map "D:\\musicradar-drum-samples\\musicradar-drum-samples\\Drum Kits" true))



                                        ;Patterns

;; (defonce beat (atom {}))

(swap! beat
       (fn [_] (gen-pattern 20
                           (conj
                            (vals (drum-kits :Kit13-Acoustic))
                            (vals (drum-kits :Kit10-Vinyl)))
                           )))


(defonce electro-beat (atom {}))
(swap! electro-beat
       (fn [_] (gen-pattern 20
                           (conj
                            (vals (drum-kits :Kit4-Electro))
                            (vals (drum-kits :Kit6-Electro)))
                           )))



;; (def beat-player (s/gets 8))
;; (s/set-source beat-player ))
;; (s/addp beat-player beat)
;; (s/addp beat-player electro-beat)
;; (s/rmp beat-player beat)
;(s/kill-sequencer beat-player)

;; (start-recorder (vals (drum-kits :Kit7-Electro)))



;;Functions to generate patterns


(defn gen-pattern [size instruments]
  (let [pattern {}]
    (reduce
     (fn [pattern beat]
       (let [instrument (choose instruments)
             scale (scale (choose [:Bb4 :C#5]) (choose [:major :minor]))
             ]
         (assoc pattern
                beat
                (vector
                 instrument
                 []
                 (if (= instrument dance-kick)
                   [:decay (- 2 (rand 1.5))
                    ;:freq (midicps (choose scale))
                    ]
                   [])
                 )
                )))
     {}
     (range 1 (inc size))
     )
    )
  )
