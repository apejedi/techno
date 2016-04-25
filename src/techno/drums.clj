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

(defonce beat (atom {}))
(swap! beat (fn [_]
              (let [sounds (drum-kits :Kit6-Electro)
                    key (-> sounds keys first name)
                    prefix (.substring key 0 (inc (.indexOf key "-")))
                    s (fn [& ins]
                        (mapcat #(vector (sounds (keyword (str prefix % ".wav"))) []) ins))]
                {2  (s "Tom02" "Kick01")
                 4  (s "PdHat" "Kick02")
                 4.25 (s "Tom05")
                 5  (s "Tom03" "Tom04")
                 6  (s "OpHat")
                 7.75 (s "Snr01")
                 8  (s "Snr02" "Clap02" "Tom05")
                                        ;1 [dance-kick []]
                 ;8.5 (s "Tom05")
                 }
                )
              ))


(defonce electro-beat (atom {}))
(swap! electro-beat
       (fn [_] (gen-pattern 20
                           (vals (drum-kits (keyword (choose
                                                      (filter #(.contains (str %) "Electro") (keys drum-kits))))))
                           )))



(comment
  (start-recorder (mapcat vals
                          (vals (group-samples (drum-kits :Kit6-Electro)))))
  (record-time-pattern 100)
  (def b1 (get-time-pattern))
  (def beat-player (s/gets 8 0.25))
  (def beat-player (s/gcs (latch:kr (env-gen:kr 8 16) (impulse:kr 1/3))))
  (s/setsp beat-player 8)
  (s/set-size beat-player 8)
  (s/addp beat-player electro-beat)
  (s/addp beat-player beat)
  (s/addp beat-player electro-beat)
  (kill beat-player)
  (stop)
  (ctl beat-player :reset 1)
  (start-recorder (vals (drum-kits :Kit7-Electro)))
  )
