(ns techno.sketches
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.synths]
        [techno.drum-patterns]
        [techno.drums]))

(comment
  (let [n 1
        comps (into [] on)]
    (s/add-p core/player (second (nth comps n))  (first (nth comps n)))
    )
  (let [comp minipops]
    (apply s/play-p (conj (vec (vals (select-keys comp [:a :b :c]))) 2))
    )
  )
(def mystery
  {:a (let [a (concat (mapcat #(vector % [:amp 0.2]) (chord :G4 :minor)) [:A4])
        b (concat (mapcat #(vector % [:amp 0.2]) (chord :G4 :minor)) [:Bb4])
        c (concat (mapcat #(vector % [:amp 0.2]) (chord :F4 :minor)) [:G4])
        d (concat (mapcat #(vector % [:amp 0.2]) (chord :F4 :minor)) [:Bb4])]
    (s/phrase-p
     piano
     [a b a b a b a b a b
      c d c d c d c d c d c d]
     0.25
     0
     [:coef 0.01]))
   }
  )

(def arpeggi
  {:a (s/phrase-p
       ks1
       [:G4 :A4 :C#5 :B4 :A4 :G4 :D5]
       0.25
       0 [:atk 0.001 :amp 0.6 :coef 0.01])
   :b (let [a [:D5 :G4 :E4]
        b [:E5 :A4 :F#4]
        c [:A5 :D5 :B4]
        d [:F#5 :D5 :B4]]
        (s/phrase-p
         klang-test
         (concat a a a a a a
                 b b b b b b
                 c c c c c c
                 d d d d d d)
         0.25 0 [:coef 0.01 :amp 0.8 :atk 0.01 :dur 2]))
   :c (s/phrase-p
       flute
       [:F#5 :G5 :A5 :4]
       0.25 1 [:dur 0.7 :amp 0.2])
   :d (let [d :D4 e :E4 a :A4 g :G4 f :F#4]
        (s/phrase-p
         bpfsaw
         [d d d d e e e e a a a a g g g g f f f f] 0.25 0 [:dur 1.5 :amp 0.5]))
   }
  )
(def r-prog
  {:a (let [root (note :Eb5)
            a [root (+ root 2)]
            b [root (+ root 3)]
            c [(dec root) (+ (dec root) 2)]
            d [(dec root) (+ (dec root) 3)]
            e [(- root 2) (+ (- root 2) 2)]
            f [(- root 3) (+ (- root 2) 2)]
            pattern [a a a b b a a b b a a a
                     c c c d d c c d d c c c
                     e e e f f f f f]
            ;pattern (map #(map midi->hz %) pattern)
            ]
     (s/phrase-p
      ks1
      pattern
      0.25 0 [:coef 0.01 :atk 0.5 :dur 0.5])
     )})

(def scatterbrain
  {:a (s/phrase-p
    piano
    [:A3 :A4 :E5 :1
     :G3 :G4 :D5 :1
     :F3 :F4 :C5 :1
     :E3 :E4 :B4 :1
     :E3 :E4 :A4 :1
     :E3 :E4 :G4 :1
     :E3 :E4 :A4 :1 :B4]
    0.25 0)}
  )

(def track2
  {:a (s/phrase-p
    overpad
    [[:E4 :G4 :B4 :D5] :24
     [:D4 :F4 :A4] :22]
    0.25 0 [:attack 2 :release 5 :amp 0.2])
   :b (s/phrase-p
    bpfsaw
    [[:D5 :F4] [:E5 :G4] :1 [:D5 :F4] [:E5 :G4] :2
     [:D5 :F4] [:E5 :G4] :3]
    0.25 0 [:amp 0.5 :atk 0.01])
   :c {
    1 [whistle [:freq1 (midi->hz (note :D5)) :freq2 (midi->hz (note :A6)) :dur 3 :amp 0.3]]
    8 [whistle [:freq1 (midi->hz (note :A6)) :freq2 (midi->hz (note :E5)) :dur 3 :amp 0.3]]
    12.75 []}
   :d (build-from-kits
    [:Kit3-Acoustic :Kit10-Vinyl]
    [[[dub-kick [100 :amp 0.8]]] :1 ["Perc03"] :1])
   :e (let [a [:amp 0.4]]
           (build-from-kits
            [:Kit3-Acoustic]
            [["SdSt-03"] :2
             ["SdSt-06"] ["SdSt-07"] :1 ["SdSt-03" "Snr-04"] :1]
            0.25 [:amp 0.4]))}
  )

(def on
  {:a (let [a [:delay-time 0.7 :decay 8]]
          (s/phrase-p
           reverb-test
           [:D5 :2 :C5 :2 :A4 :3
            [:G4 :D4] :1 [:A4 :E4] :C5
            [:F4 :G4] a [:F4 :G4] a [:F4 :G4] a :3]
           0.25 0 [:delay-time 0.4 :decay 7]))
   :b (s/phrase-p
       piano
       [:E4 :3 :A4 :3 :G4 :7]
       0.25 0 [:attack 2 :release 1 :amp 0.4 :muffle 2])
   :c (s/phrase-p
       overpad
       [[:D4 :F4 :A4] :4 [:F4 :A4 :C5]]
       0.25 0 [:attack 2 :release 1])
   }
  )
(def bass-line
  (let [d :D2]
      (s/phrase-p
       plk-bass
       [:A3 :G2 :C3]
       0.25
       0
       [:dur 1 :amp 0.8]))
  )
(def melissa
  {:a (let [l [:dur 3]
         a [:Ab2 l]
         f [:F#2 l]
         main [:Eb3 :Bb4 :Eb3 :Bb4 l [:space 1] :Eb3 :Bb4]]
     (s/phrase-p
      ks1
      (concat a main
              a main
              f main
              f main
              [:Bb4] [:C5 l])
      (double (/ 1 4))
      0
      [:dur 2 :atk 0.001 :coef 0.01 :amp 0.5]
      )
     )
   :b (s/phrase-p
       bpfsaw
       [:C6 :Bb5 :G5 :2
        :F5 :Eb5 :F5 :2
        :Ab5 :2
        :Bb5 :G5 :Bb5 :2
        :C6 :Bb5 :F5 :Bb5 :3
        ]
       (double (/ 1 4))
       1
       [:dur 2 :atk 0.001 :amp 0.5]
       )
   :c (fn [b] (let [pat
                   (build-from-kits
                    [:Kit3-Acoustic]
                    {0 ["SdSt-04"]
                     0.25 ["SdSt-04"]
                     0.5 ["Snr-06"]
                     0.75 []
                     })]
               (pat (- b (int b)))
               ))
   }
  )

(def track1
  {:a (let [get-p (fn [d]
                    (s/chord-p
                     sweet
                     (chord-degree d :C4 :minor 4)
                     [:amp 0.2 :dur 1 :coef 0.01 :attack 1 :release 2]))
            prog {1 (get-p :vi)
                  2 (get-p :v)
                  3 (get-p :iv)
                  4 (get-p :ii)
                  4.75 []
                  }]
        prog
        )
   :b (let [root :C4
            type :minor
            args [:coef 0.001 :amp 0.4 :atk 0.01 :dur 1]
            inst bpfsaw
            v (flatten (repeat 8 (chord-degree :v root type 4)))
            i (flatten (repeat 8 (chord-degree :iv root type 4)))]
        (s/arp-p inst (concat v i) args 0)
        )
   :c (fn [b]
        (let [n1 (choose (scale :C5 :minor))
              n2 (choose (scale :C5 :minor))]
          (if (or (= (- b (int b)) 0.5)
                  (= (rand-int 2) 1)
                                        ;true
                  )
            [
                                        ;overpad [n1 :amp 0.3 :dur 2 :attack 0.1 :release 0.3]
             piano [n2 :amp 0.4 :dur 1]
             flute [n2 :amp 0.2 :dur 1 :coef 0.001]
             ])
          )
        )
   :main1 beat
   :main2 (:bomba @beats)
   :main3 (:four-beat @beats)
   })
(def x-naut
  {:a (let [a [:Eb5 :G5 :Bb5 :D6 :Bb5 :Eb5]
            b [:Eb5 :G5 :Ab5 :C6 :Ab5 :Eb5]
            c [:Eb5 :Eb5 :F5 :F5 :G5 :G5 :G5 :G5 [:dur 0.2] :1 :G5 :G5 :G5]]
             (s/merge-p
              (s/phrase-p
               sin-inst
               (concat a a a a a
                       b b b b b
                       a a c)
               0.25 0 [:attack 0.1 :release 0.1 :dur 0.1])
              ))
   ;; :b (s/phrase-p
   ;;     overpad
   ;;     [(chord :D4 :minor) :16 [:C4 :E4 :A4] :16]
   ;;     0.25 0 [:attack 2 :release 3])
   }
  )

(def minipops
  {:a (s/phrase-p
       piano
       [:Ab4 :Ab4 :A4 :E4 :E4 :3
        :E4 :E4 :F4 :G4 :G4 :3
        :G4 :G4 :2 :G4 :0 :F4 :0 :D4 :0 :D4 :2
        :D4 :D4 :A4 :E4 :E4]
       0.25 1 [:dur 3])
   }
  )
