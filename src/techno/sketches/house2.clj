(ns techno.house2
  (:import java.util.concurrent.ThreadLocalRandom)
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.synths]
        [techno.drum-patterns]
        [techno.drums]
        [techno.controller]))
(comment
  (do (swap!
       (:harmony @sketch)
       (fn [_]
         (s/phrase-p
          rise-fall-pad
          [;(map midi->hz (chord :C3 :M7))
           (map midi->hz (chord :F3 :M7))
           (map midi->hz (chord :E3 :m7))
           :34
           ]
          0.25 32 [:t 5]))) nil)
  (s/set-arg @core/s-player :harmony :amp 1)

  (do (swap!
       (:motif @sketch)
       (fn [_]
         (s/phrase-p
          bpfsaw
          (conj (vec (chord-degree :iv :C4 :major 4)) :0)
          0.25 0 [:coef 0.01 :dur 0.3 :amp 0.5])
         ;; (s/phrase-p
         ;;  bass-synth
         ;;  [[:E3 :B3] [:A3 :F3] :34]
         ;;  0.25 32 [:release 6 :amp 0.4 :detune 4])
         )) nil)
  (s/set-arg @core/s-player :motif :amp 1)

  (do (swap!
       (:motif2 @sketch)
       (fn [_]
         (s/phrase-p
          bass-synth
          [[:E4 :B3] [:A4 [:amp 0.3] :F4 [:amp 0.3]] :34]
          0.25 32 [:release 6 :amp 0.4 :detune 4]))) nil)
  (s/set-arg @core/s-player :motif2 :amp 1)

  (do (swap!
       (:motif3 @sketch)
       (fn [_]
         (s/phrase-p
          reverb-test
          [:A4 :B4 :D4 :C4 :B4]
          0.25 2))) nil)
  (s/set-arg @core/s-player :motif3 :amp 1)

  (do (swap!
       (:kick @sketch)
       (fn [_]
         (drum-pattern
          [:Kit4-Electro :Kit3-Acoustic]
          [[k1] :1 [o1] :1]
          0.25))) nil)
  (s/set-arg @core/s-player :kick :amp 1)

  (do (swap!
       (:cl @sketch)
       (fn [_]
         (drum-pattern
          [:Kit16-Electro]
          [[c1] [c1] [c2] :2 [c1] :2]
          0.25 [:amp 0.3]))) nil)
  (s/set-arg @core/s-player :cl :amp 1)

  (do (swap!
       (:snr @sketch)
       (fn [_]
         (drum-pattern
          [:Kit3-Acoustic :Kit16-Electro :Kit4-Electro]
          [:2 [s3] :1]
          0.25))) nil)
  (s/set-arg @core/s-player :snr :amp 1)

  (do (swap!
       (:t @sketch)
       (fn [_]
         (drum-pattern
          [:Kit5-Electro]
          [[cl1] :2]
          0.25))) nil)
  (s/set-arg @core/s-player :t :amp 1)

  )
