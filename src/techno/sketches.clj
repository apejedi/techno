(ns techno.sketches
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.synths]
        [techno.drum-patterns]
        [techno.drums]))

(def mystery
  (let [a (concat (mapcat #(vector % [:amp 0.2]) (chord :G4 :minor)) [:A4])
        b (concat (mapcat #(vector % [:amp 0.2]) (chord :G4 :minor)) [:Bb4])
        c (concat (mapcat #(vector % [:amp 0.2]) (chord :F4 :minor)) [:G4])
        d (concat (mapcat #(vector % [:amp 0.2]) (chord :F4 :minor)) [:Bb4])]
    (s/phrase-p
     piano
     [a b a b a b a b a b
      c d c d c d c d c d c d]
     0.25
     0
     [:coef 0.01])))
(def mystery-b
  (fn [b]
    (if (= (rand-int 3) 0)
      [overpad [(choose (map note [:A5 :Bb5 :F5 :G5])) :attack 1]])
    ))

(def arpeggi
  (s/phrase-p
   bpfsaw
   [:G4 :A4 :C#5 :B4 :A4 :G4 :D5]
   0.25
   0 [:atk 0.1 :amp 0.4]))
(def arpeggi-b
  (let [a [:D5 :G4 :E4]
        b [:E5 :A4 :F#4]
        c [:A5 :D5 :B4]
        d [:F#5 :D5 :B4]]
    (s/phrase-p
     piano
     (concat a a a a a a
             b b b b b b
             c c c c c c
             d d d d d d)
     0.25 0 [:coef 0.01 :amp 0.2])))
(def arpeggi-d
  (let [d :D4 e :E4 a :A4 g :G4 f :F#4]
    (s/phrase-p
     sin-inst
     [d d d d e e e e a a a a g g g g f f f f] 0.25 1 [:dur 1.5 :amp 0.5]))
  )
(s/add-p core/player arpeggi-d)
;(s/play-p arpeggi arpeggi-b arpeggi-c 1.2)
(def arpeggi-c
  (s/phrase-p
   flute
   [:F#5 :G5 :A5 :4]
   0.25 1 [:dur 0.7 :amp 0.2]))
(def r-prog
  (let [root (note :Eb5)
        a [root (+ root 2)]
        b [root (+ root 3)]
        c [(dec root) (+ (dec root) 2)]
        d [(dec root) (+ (dec root) 3)]
        e [(- root 2) (+ (- root 2) 2)]
        f [(- root 3) (+ (- root 2) 2)]]
    (s/phrase-p
     bpfsaw
     [a a a b b a a b b a a a
      c c c d d c c d d c c c
      e e e f f f f]
     0.25 0 [:coef 0.01 :atk 0.01])
    ))


(def track2
  {:harmony (s/phrase-p
    overpad
    [[:E4 :G4 :B4 :D5] :24
     [:D4 :F4 :A4] :22]
    0.25 0 [:attack 2 :release 5 :amp 0.2])
   :motif (s/phrase-p
    bpfsaw
    [[:D5 :F4] [:E5 :G4] :1 [:D5 :F4] [:E5 :G4] :2
     [:D5 :F4] [:E5 :G4] :3]
    0.25 0 [:amp 0.5 :atk 0.01])
   :motif2 {
    1 [whistle [:freq1 (midi->hz (note :D5)) :freq2 (midi->hz (note :A6)) :dur 3 :amp 0.3]]
    8 [whistle [:freq1 (midi->hz (note :A6)) :freq2 (midi->hz (note :E5)) :dur 3 :amp 0.3]]
    12.75 []}
   :main (build-from-kits
    [:Kit3-Acoustic :Kit10-Vinyl]
    [[[dub-kick [100 :amp 0.8]]] :1 ["Perc03"] :1])
   :t (let [d [zap [:amp 0.1] dub-kick [300 :amp 1.5]]]
             (s/build-rest-p
              [d :6 d :2 d :5])
             )}
  )
