(ns techno.sketches-live
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
  (s/set-arg @core/s-player :sdst :amp 0.5)
  (swap!
   (:harmony @sketch)
   (fn [_]
     (s/phrase-p
      overpad
      [[:E4 :G4 :B4 :D5] :24
       [:D4 :F4 :A4] :22]
      0.25 0 [:attack 2 :release 5 :amp 0.1])))
  (swap!
   (:bells @sketch)
   (fn [_]
     (s/phrase-p
      bpfsaw
      [[:D5 :F4] [:E5 :G4] :1 [:D5 :F4] [:E5 :G4] :2
       [:D5 :F4] [:E5 :G4] :3]
      0.25 0 [:amp 0.5 :atk 0.01])))
  (swap!
   (:whistle @sketch)
   (fn [_]
     (midi->hz (note :D5))))
  (swap!
   (:freq2 @sketch)
   (fn [_]
     (midi->hz (note :A6))))
  (swap!
   (:dur @sketch)
   (fn [_]
     (midi->hz (note :A6))))
  (swap!
   (:freq2 @sketch)
   (fn [_]
     (midi->hz (note :E5))))
  (swap!
   (:dur @sketch)
   (fn [_]
     (let [k "Kick-01" s "Snr03"]
       (build-from-kits
        [:Kit3-Acoustic :Kit16-Electro]
        [[k [dub-kick [100 :amp 1]]] :3]
        0.25))))
  (swap!
   (:sdst @sketch)
   (fn [_]
     (let [c1 "ClHat02" c2 "ClHat01" a [:amp 0]]
       (build-from-kits
        [:Kit16-Electro]
        [[c1] :2 [c1] :2 [c1] :1 [c1] [c2] [c1] :5]
        0.25))))
  (swap!
   (:hat @sketch)
   (fn [_]
     (let [o "OpHat"]
       (drum-pattern
        [:Kit16-Electro]
        [:2 [o] :1]
        0.25))))
  (swap!
   (:sdst2 @sketch)
   (fn [_]
     (let [s3 "SdSt-03"
           s6 "SdSt-06"
           s7 "SdSt-07"
           s4 "SdSt-04"]
       (drum-pattern
        [:Kit3-Acoustic]
        [[s6] :2 [s6] :2 [s6] :1 [s6] [s7] [s6] :5]
        0.25 [:amp 0.6]))))
  )
