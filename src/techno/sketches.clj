(ns techno.sketches
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.synths]))

(def mystery
  (let [a (concat (mapcat #(vector % [:amp 0.2]) (chord :G4 :minor)) [:A4])
        b (concat (mapcat #(vector % [:amp 0.2]) (chord :G4 :minor)) [:Bb4])
        c (concat (mapcat #(vector % [:amp 0.2]) (chord :F4 :minor)) [:G4])
        d (concat (mapcat #(vector % [:amp 0.2]) (chord :F4 :minor)) [:Bb4])]
    (s/phrase-p
     ks1
     [a b a b a b a b a b
      c d c d c d c d c d c d]
     0.25
     0
     [:coef 0.01])))
(def mystery-b
  (fn [b]
    (if (= (rand-int 3) 0)
      [bpfsaw [(choose (map note [:A5 :Bb5 :F5 :G5])) :dur 2]])
    ))
