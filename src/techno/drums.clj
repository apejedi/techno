(ns techno.drums
  (:use [overtone.core]
        [overtone.inst.drum]
        [overtone.inst.synth]
        [techno.core :as core]
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
    (cond (map? pattern) (zipmap (keys pattern) (map s (vals pattern)))
          (sequential? pattern) (map s pattern)
          )
    )
  )


                                        ;Patterns




(defonce beat (atom {}))
(swap! beat (fn [_]
              (build-from-kits [:Kit10-Vinyl]
                               {
                                1 ["Kick01" "ClHat01"]
                                1.25 ["Kick04"]
                                1.5 ["Perc04"]
                                2 ["Snr02"]
                                2.5 ["Perc03"]
                                ;3 ["Perc02"]
                                4 ["Snr02"]
                                4.75 []
                                }
                              ;;  {1 ["Kick05" "Kick04"]
                              ;; 1.5 ["Perc01"]
                              ;; 1.75 ["Kick01"]
                              ;; 2 ["Kick02"]
                              ;; 2.25 ["Perc01"]
                              ;; 2.5 ["Kick01"]
                              ;; 2.75 []
                              ;; }
                               )
              ))


(defonce syncop (atom nil))
(swap! syncop (fn [_]
                (fn [b]
                  (if (or (= (rand-int 3) 1) (integer? b))
                    [(get-in drum-kits [:Kit8-Vinyl :CYCdh_VinylK1-Tamb.wav]) []]
                    )
                  )
                ))

(defonce electro (atom {}))
(swap! electro
       (fn [_]
         (build-from-kits [:Kit10-Vinyl :Kit15-Electro]
                          {
                           1 ["Perc01"]
                           1.5 ["ClHat01"]
                           })
         ))

(defonce pulse-beat (atom []))
(swap! pulse-beat (fn [_]
                    (build-from-kits [:Kit10-Vinyl]
                          {
                           1 ["Perc02"]
                           1.25 ["Perc03"]
                           2 ["Perc04"]
                           2.75 []
                           }
                          )))



(comment
  (start-recorder (mapcat vals
                          (vals (group-samples (drum-kits :Kit10-Vinyl)))))
  (def beat-player (s/get-s (/ 80 60) 0.25))
  (s/set-sp core/player (/ 80 60))
  (s/add-p core/player beat :main)
  (s/add-p core/player electro :electro)
  (s/add-p core/player pulse-beat :pulse)
  (s/add-p core/player (:bomba @beats) :main)
  (s/add-p core/player (:four-beat @beats) :main)
  (s/add-p core/player syncop :syncop)

  (s/rm-p core/player :syncop)
  (s/rm-p core/player :main)
  (s/wrap-p core/player :electro)

  (stop)
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
