(ns techno.track2
  (:import java.util.concurrent.ThreadLocalRandom)
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.synths]
        [techno.drum-patterns]
        [techno.drums]
        [techno.controller]))

(s/set-sp @core/s-player 2)


(comment
  (do (swap!
       (:harmony @sketch)
       (fn [_]
         (s/phrase-p
          overpad
          [[:E4 :G4 :B4 :D5] :24
           [:D4 :F4 :A4] :22]
          0.25 0 [:attack 2 :release 5 :amp 0.2]))) nil)
  (s/set-arg @core/s-player :harmony :amp 1)

  (do (swap!
       (:bells @sketch)
       (fn [_]
         (s/phrase-p
          bpfsaw
          [[:D5 :F4] [:E5 :G4] :1 [:D5 :F4] [:E5 :G4] :2
           [:D5 :F4] [:E5 :G4] :3]
          0.25 0 [:amp 0.5 :atk 0.01]))) nil)
  (s/set-arg @core/s-player :bells :amp 1)

  (do (swap!
       (:whistle @sketch)
       (fn [_]
         (let [a [:freq1 (midi->hz (note :D5)) :freq2 (midi->hz (note :A6)) :dur 3 :amp 0.2]
               b [:freq1 (midi->hz (note :A6)) :freq2 (midi->hz (note :E5)) :dur 3 :amp 0.2]]
           (s/build-rest-p
            [[whistle a] :27 [whistle b] :19]
            0.25 0)))) nil)
  (s/set-arg @core/s-player :whistle :amp 0.7)

  (do (swap!
       (:kicks @sketch)
       (fn [_]
         (let [k "Kick-01" s "Snr03"]
           (build-from-kits
            [:Kit16-Electro]
            [[k [dub-kick [100 :amp 1]]] :3]
            0.25)))) nil)
  (s/set-arg @core/s-player :kicks :amp 1)

  (do (swap!
       (:sdst @sketch)
       (fn [_]
         (let [c1 "ClHat02" c2 "ClHat01" a [:amp 0]]
           (build-from-kits
            [:Kit16-Electro]
            [[c1] :2 [c1] :2 [c1] :1 [c1] [c2] [c1] :5]
            0.25)))) nil)
  (s/set-arg @core/s-player :sdst :amp 1)

  (do (swap!
       (:hat @sketch)
       (fn [_]
         (drum-p [:Kit16-Electro]
                 [:2 :o :1]))) nil)
  (s/set-arg @core/s-player :hat :amp 1)

  (do (swap!
       (:kick2 @sketch)
       (fn [_]
         (s/m-phrase
          {:refresh 0 :sputter 0.3 :sputter-amt 0.4 :reverse 0}
          (drum-p [:Kit4-Electro] [:k2 :3 :k2 :3 :k2 :k2 :2 :k2 :3]) 0.25)
         ;; (drum-p [:Kit4-Electro] [:k2 :3 :k2 :3 :k2 :k2 :2 :k2 :3])
         )
       ) nil)
  (s/set-arg @core/s-player :kick2 :amp 1)

  (do (swap!
       (:clap @sketch)
       (fn [_]
         (drum-p [:Kit16-Electro] [:12 [:cl1 :cl2] :3]))) nil)
  (s/set-arg @core/s-player :clap :amp 1)

  (do (swap!
       (:zap @sketch)
       (fn [_]
         (let [z1 [zap [(midi->hz (note :Eb3)) (midi->hz (note :Eb2)) :dur 0.2 :amp 2]]
               z2 [zap [(midi->hz (note :F3)) (midi->hz (note :F2)) :dur 0.2 :amp 2]]]
           (drum-p [:Kit4-Electro] [z1 :1 z2 z2 :4])))) nil)
  (s/set-arg @core/s-player :zap :amp 1)

  (do (swap!
       (:motif @sketch)
       (fn [_]
         (let [;a [:D4 :E4 :C5]
               a [:D4 :E4 :B4]
               b [:C4 :D4 :A4]
               ]
           (s/phrase-p
            reverb-test
            (concat a a a a a a a a b b b b b b b b)
            0.25 0 [:amp 1 :t 2])))) nil)
  (s/set-arg @core/s-player :motif :amp 1)

  )
