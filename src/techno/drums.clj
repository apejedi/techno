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
                   (drum-kits (keyword (choose (filter #(.contains (str %) "Acoustic") (keys drum-kits))))))
                  ))

(defonce beat (atom {}))
(swap! beat (fn [_]
              ;; (fn [beat]
              ;;   (let [get-if #(if (contains? @drum-kit %) % (choose (keys @drum-kit)))
              ;;         type-map {1 :Kick 3 :ClHat 4 :Snr 6 :Kick 7 :ClHat 8 :Tom}
              ;;         action (vector (choose (vals (@drum-kit (get-if (type-map (int beat)))))) [])]
              ;;                           ;(println beat " " action)
              ;;     action
              ;;     )
              ;;   )
              ;; (map #(vector (get-in drum-kits [:Kit6-Electro %]) [])
              ;;      (map #(keyword (str "CYCdh_ElecK03-" % ".wav") )
              ;;           ["Tom03" nil "Snr02" nil "Tom02" "Tom03" nil "Snr02" nil]))
              (let [kit "CYCdh_ElecK02-"
                    sounds (drum-kits :Kit5-Electro)
                    sound #(vector (sounds (keyword (str kit % ".wav"))) [])]
                [(sound "FX03")]
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
  (def beat-player (s/gbs b1 200))
  (s/setsp beat-player 8)
  (s/set-size beat-player 8)
  (s/addp beat-player electro-beat)
  (s/addp beat-player beat)
  (s/rmp beat-player electro-beat)
  (kill beat-player)
  (start-recorder (vals (drum-kits :Kit7-Electro)))

  )
