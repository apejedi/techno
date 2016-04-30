(ns techno.drums
  (:use [overtone.core]
        [overtone.inst.drum]
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
                       (second (first
                                (filter (fn [[k v]] (.contains (name k) in)) sounds)))
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
                                5 []
                                }
                               )
              ))

(defonce beat2 (atom {}))
(swap! beat2 (fn [_]
               (build-from-kits [:Kit10-Vinyl]
                                {
                                 1 ["Kick01"]
                                1.5 ["Perc03"]
                                1.75 ["Perc02"]
                                2 ["Kick03" "Snr02"]
                                2.5 ["Crash-02"]
                                3 ["Kick01"]
                                3.5 ["HfHat02"]
                                4 ["Kick03" "Snr02"]
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
                           1.25 ["ClHat02" "Perc01"]
                           1.5 []
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
                          (vals (group-samples (drum-kits :Kit6-Electro)))))
  (record-time-pattern 100)
  (s/play 1 @pulse-beat)
  (def b1 (get-time-pattern))
  (def beat-player (s/get-s 2 0.25))
  (s/set-sp beat-player 2)
  (s/set-st beat-player 0.25)
  (node-get-control beat-player :pattern-size)
  (s/set-size beat-player 2)
  (s/add-p beat-player electro-beat :electro)
  (s/add-p beat-player beat :beat)
  (s/add-p beat-player beat2)
  (s/rm-p beat-player :beat)
  (s/wrap-p beat-player electro-beat)
  (kill beat-player)
  (stop)
  (ctl beat-player :reset 1)
  (start-recorder (vals (drum-kits :Kit7-Electro)))
  )
