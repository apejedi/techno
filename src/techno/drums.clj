(ns techno.drums
  (:use [overtone.core]
        [overtone.inst.drum]
        [overtone.inst.synth]
        [techno.samples]
        [techno.recorder]
        )
  (:require [techno.sequencer :as s])
  )


(def drum-kits (create-sample-map "D:\\musicradar-drum-samples\\musicradar-drum-samples\\Drum Kits" true))



;;Functions to generate patterns


(defn gen-pattern [size instruments]
  (let [pattern {}]
    (reduce
     (fn [pattern beat]
       (let [instrument (choose instruments)]
         (assoc pattern
                beat
                (vector instrument [])
                )))
     {}
     (range 1 (inc size))
     )
    )
  )




                                        ;Patterns

(defonce drum-kit (atom {}))
(swap! drum-kit (fn [_]
                  (group-samples
                   (drum-kits (keyword (choose (filter #(.contains (str %) "Electro") (keys drum-kits))))))
                  ))

(defn build-from-kits [kits pattern]
  (let [sounds (reduce #(merge %1 (drum-kits %2)) {} kits)
        s (fn [ins]
            (mapcat (fn [in]
                      (vector
                       (if (string? in)
                         (second (first
                                  (filter (fn [[k v]] (.contains (name k) in)) sounds)))
                         in)
                       []))
                    ins))
        ]
    (zipmap (keys pattern) (map s (vals pattern)))
    )
  )
(defonce beat (atom {}))
(swap! beat (fn [_]
              (build-from-kits [:Kit10-Vinyl]
                               {
                                1 ["Kick01" "ClHat01"]
                                1.25 ["Kick04"]
                                1.5 ["Perc04"]
                                2 ["Snr02"]
                                2.5 ["Perc03"]
                                3 ["Perc02"]
                                4 ["Snr02"]
                                4.75 []
                                }
                               )
              ))



(defonce electro-beat (atom {}))
(swap! electro-beat
       (fn [_]
         (build-from-kits [:Kit10-Vinyl]
                          {
                           1 ["ClHat01"]
                           1.25 ["Perc01"]
                           1.5 ["ClHat01"]
                           ;1.75 []
                           })
         ))

(defonce pulse-beat (atom []))
(swap! pulse-beat (fn [_]
                    (fn [b]
                      (if (and (integer? b) (odd? b))
                        [dance-kick [:amp 0.3]]
                        )
                      )
                    ))



(comment
  (start-recorder (mapcat vals
                          (vals (group-samples (drum-kits :Kit10-Vinyl)))))
  (def beat-player (s/get-s 2 0.25))
  (s/set-sp beat-player 1)
  (s/set-st beat-player 0.25)
  (node-get-control beat-player :pattern-size)
  (s/set-size beat-player 1.5)
  (s/add-p beat-player electro-beat :electro)
  (s/add-p beat-player beat :beat)

  (s/rm-p beat-player :beat)
  (s/wrap-p beat-player electro-beat)
  (kill beat-player)
  (stop)
  (ctl beat-player :reset 1)
  (start-recorder (vals (drum-kits :Kit7-Electro)))
  )


(defonce beats (atom {}))
(swap! beats (fn [_]
               {:one-two
                (build-from-kits [:Kit10-Vinyl]
                                 {
                                  1 ["Kick01"]
                                  2 ["Snr01"]
                                  3 []
                                  }
                                 )
                :three-beat (build-from-kits
                             [:Kit10-Vinyl]
                             {1 ["Kick01"]
                              1.75 ["Snr01"]
                              2.25 ["Kick01"]
                              3 []
                              })
                :four-beat (build-from-kits
                             [:Kit10-Vinyl]
                             {1 ["Kick01"]
                              1.5 ["ClHat01"]
                              2 ["Snr02"]
                              2.5 ["ClHat01"]
                              2.75 []
                              })
                :three-four (build-from-kits
                             [:Kit10-Vinyl]
                             {1 ["Kick04"]
                              1.75 ["Snr02"]
                              2.25 ["Kick04"]
                              2.75 ["Snr02"]
                              })
                :six-eight (build-from-kits
                             [:Kit10-Vinyl]
                             {1 ["Snr02"]
                              1.75 ["Kick04"]
                              2 ["Snr02"]
                              2.25 ["Kick01"]
                              2.75 []
                              })
                :bomba (build-from-kits
                             [:Kit10-Vinyl]
                             {1 ["Kick01" "Kick04"]
                              1.5 ["Perc01"]
                              1.75 ["Kick01"]
                              2 ["Kick02"]
                              2.25 ["Perc01"]
                              2.5 ["Kick01"]
                              2.75 []
                              })
                }
               ))

(loop [times []]
  (if (>= (count times) 2)
    (do (println (- (second times) (first times)))
        (recur (rest times)))
    )
  )
