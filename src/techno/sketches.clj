(ns techno.sketches
  (:import java.util.concurrent.ThreadLocalRandom)
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.melody]
        [techno.samples]
        [techno.synths]
        [techno.drum-patterns]
        [techno.drums]))

(comment
  (let [parts [:bells]
        rm []
        comp track2]
    (doseq [p rm]
      (s/rm-p core/player p)
      )
    (doseq [p parts]
      (s/add-p core/player (comp p) p)
      )
    )

  (let [comp song-of-storms
        parts (keys comp)]
    (apply s/play-p (conj (vec (vals (select-keys comp parts))) 1.3))
    )
  )

(def mystery
    {:a (let [a (concat (mapcat #(vector % [:amp 0.2]) (chord :G4 :minor)) [:A4])
              b (concat (mapcat #(vector % [:amp 0.2]) (chord :G4 :minor)) [:Bb4])
              c (concat (mapcat #(vector % [:amp 0.2]) (chord :F4 :minor)) [:G4])
              d (concat (mapcat #(vector % [:amp 0.2]) (chord :F4 :minor)) [:Bb4])]
          (s/phrase-p
           flute
           [a b a b a b a b a b
            c d c d c d c d c d c d]
           0.25
           0
           [:coef 0.01 :dur 0.5 :attack 0.01]))
     }
    )

(def template
  {:drum1 (drum-p [:Kit15-Electro] [])
   :drum2 (drum-p [:Kit16-Electro] [])
   :drum3 (drum-p [:Kit9-Vinyl] [])
   :drum4 (drum-p [:KurzweilKit08] [])
   :motif1 (s/phrase-p
            bpfsaw
            []
            0.25 0 [])
   :motif2 (s/phrase-p
            overpad
            []
            0.25 0 [])
   :motif3 (s/phrase-p
            bass2
            []
            0.25 0 [])
   :motif4 (s/phrase-p
            plk-bass
            []
            0.25 0 [])
   :motif5 (s/phrase-p
            bpfsaw
            []
            0.25 0 [])
   :harmony1 (s/phrase-p
            bpfsaw
            []
            0.25 0 [])
   :harmony2 (s/phrase-p
            overpad
            []
            0.25 0 [])
   :harmony3 (s/phrase-p
            bass2
            []
            0.25 0 [])
   :harmony4 (s/phrase-p
            plk-bass
            []
            0.25 0 [])
   :harmony5 (s/phrase-p
            bpfsaw
            []
            0.25 0 [])

   }
  )
  (def arpeggi
    {:a (s/phrase-p
         ks1
         [:G4 :A4 :C#5 :B4 :A4 :G4 :D5]
         0.25
         0 [:atk 0.001 :amp 0.6 :coef 0.01 :dur 1])
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
         bpfsaw
         [:F#5 :G5 :A5 :4]
         0.25 1 [:dur 0.7 :atk 0.01 :amp 1])
     :d (let [d :D4 e :E4 a :A4 g :G4 f :F#4]
          (s/phrase-p
           bpfsaw
           [d d d d e e e e a a a a g g g g f f f f]
           0.25 0 [:dur 1.5 :amp 0.5 :attack 0.5 :release 1]))
     :e (fn [b]
          (get
           {10 (s/chord-p flute [:G4 :A4 :D5] [:dur 2])
            12 (s/chord-p flute [:G4 :A4 :C#5] [:dur 2])
            14 (s/chord-p flute [:G4 :A4 :B4] [:dur 2])
            } b)
          ;; (s/phrase-p
          ;;  flute
          ;;  [[:G4 :A4 :D5] [:G4 :A4 :C#5] [:G4 :A4 :B4]]
          ;;  0.25 9 [:dur 3])
          )
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
           piano
           pattern
           0.25 0 [:coef 0.01 :atk 0.01 :dur 2])
          )})

  (def scatterbrain
    {:a (s/phrase-p
         bpfsaw
         [:A3 :A4 :E5 :1
          :G3 :G4 :D5 :1
          :F3 :F4 :C5 :1
          :E3 :E4 :B4 :1
          :E3 :E4 :A4 :1
          :E3 :E4 :G4 :1
          :E3 :E4 :A4 :1 :B4]
         0.25 0 [:atk 0.01])}
    )
(def track2
  {:harmony (s/phrase-p
             overpad
             [[:E4 :G4 :B4 :D5] :24
              [:D4 :F4 :A4] :22]
             0.25 0 [:attack 2 :release 5 :amp 0.2])
   :bells (s/phrase-p
           bpfsaw
           [[:D5 :F4] [:E5 :G4] :1 [:D5 :F4] [:E5 :G4] :2
            [:D5 :F4] [:E5 :G4] :3]
           0.25 0 [:amp 0.5 :atk 0.01])
   :whistle (let [a [:freq1 (midi->hz (note :D5)) :freq2 (midi->hz (note :A6)) :dur 3 :amp 0.3]
                  b [:freq1 (midi->hz (note :A6)) :freq2 (midi->hz (note :E5)) :dur 3 :amp 0.3]]
              (s/build-map-p
               [[whistle a] :27 [whistle b] :19]
               0.25 0))

   :kicks (let [k "Kick-01" s "Snr03"]
            (build-from-kits
             [:Kit16-Electro]
             [[k [dub-kick [100 :amp 0.6]]] :3]
             0.25))
   :sdst (let [a [:amp 0.5]]
           (drum-p
            [:Kit16-Electro]
            [:c1 a :2 :c1 a :2 :c1 a :1 :c1 a :c2 a :c1 a :5]
            0.25))
   :hat2 (s/build-map-p
          [[ (get-in drum-kits [:KurzweilKit07 :CYCdh_Kurz07-ClHat01.wav])[]]
           [ (get-in drum-kits [:KurzweilKit07 :CYCdh_Kurz07-ClHat01.wav])[]] :1
           [ (get-in drum-kits [:KurzweilKit07 :CYCdh_Kurz07-HfHat01.wav])[]] :2
           [ (get-in drum-kits [:KurzweilKit06 :CYCdh_Kurz06-ClHat.wav])[]
            (get-in drum-kits [:Kit15-Electro :CYCdh_ElecK05-ClHat04.wav])[]] :1]
          )
   :sdst2 (s/build-map-p
           [[ (get-in drum-kits [:KurzweilKit05 :CYCdh_Kurz05-SdSt.wav])[]] :2
            [ (get-in drum-kits [:KurzweilKit05 :CYCdh_Kurz05-SdSt.wav])[]] :2
            [ (get-in drum-kits [:KurzweilKit05 :CYCdh_Kurz05-SdSt.wav])[]] :3
            [ (get-in drum-kits [:KurzweilKit05 :CYCdh_Kurz05-SdSt.wav])[]] :4
            [ (get-in drum-kits [:KurzweilKit05 :CYCdh_Kurz05-SdSt.wav])[]] :1
            [ (get-in drum-kits [:KurzweilKit05 :CYCdh_Kurz05-SdSt.wav])[]] :1
            [ (get-in drum-kits [:KurzweilKit08 :CYCdh_Kurz08-SdSt01.wav])[]
             (get-in drum-kits [:KurzweilKit08 :CYCdh_Kurz08-SdSt02.wav])[]] :1]
           )
   :hat (drum-p [:Kit16-Electro]
                [:2 :o :1])
   :kick2 (drum-p [:Kit4-Electro] [:k2 :3 :k2 :3 :k2 :k2 :2 :k2 :3])
   :clap (drum-p [:Kit16-Electro] [:12 [:cl1 :cl2] :3])

   :zap (let [z1 [zap [(midi->hz (note :Eb3)) (midi->hz (note :Eb2)) :dur 0.2 :amp 2]]
              z2 [zap [(midi->hz (note :F3)) (midi->hz (note :F2)) :dur 0.2 :amp 2]]]
          (drum-p [:Kit4-Electro] [z1 :1 z2 z2 :4]))
   :motif (let [;a [:D4 :E4 :C5]
               a [:D4 :E4 :B4]
               b [:C4 :D4 :A4]
                ]
            (s/phrase-p
             reverb-test
             (concat a a a a a a a a b b b b b b b b)
             ;[a :24 b :22]
             0.25 0 [:amp 1 :t 2]))
   :clap2 (drum-p
           [:Kit15-Electro]
           [:4 :cl1 :3]
           0.25)
   :bass (s/phrase-p
          acid-bass
          [:F1 :2 :B1 :2 :B1 :9]
          0.25 0 [:dur 1 :amp 1])
   :bass2 (s/phrase-p
           acid-bass
           [:C4 :D4 :E4 :C4 :D4 :3 :E4 :F4 :E4 :D4 :6]
           0.25 2 [:dur 0.6 :amp 0.3])
   }
  )

(def on
  {:a (let [a [:delay-time 0.7 :decay 8 :amp 0.3]]
        (s/phrase-p
         reverb-test
         [:Eb5 :2 :C#5 :2 :Bb4 :3
          :Ab4 :1 :Bb4 :Ab4
          :Bb4 :1 :C#5 :1
          :F#4 a :F#4 a :F#4 a :3]
         0.25 0 [:delay-time 0.5 :decay 7 :amp 0.3]))
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
  {:a (let [c #(chord-degree % :C4 :minor)]
        (s/phrase-p
         sweet
         [(c :vi) (c :v) (c :iv) (c :ii) :3]
         0.25 3 [:amp 0.2 :dur 1 :coef 0.01 :attack 1 :release 2]))
   :a2 (let [c #(chord-degree % :C4 :minor 4)]
         (s/phrase-p
          sweet
          [(c :v) (c :ii) (c :vi) (c :i) :3]
          0.25 3 [:amp 0.2 :dur 1 :coef 0.01 :vib 0]))
   :b (let [root :C4
            type :minor
            args [:coef 0.001 :amp 0.4 :atk 0.01 :dur 1]
            inst ks1
            v (chord-degree :v root type 4)
            i (chord-degree :iv root type 4)]
        (s/phrase-p inst (flatten [v v v v v v v v
                                   i i i i i i i i]) 0.25 0 args)
        )
   :b2 (s/phrase-p
        piano
        (gen-phrase (concat (scale :C5 :minor))
                    12 :degree 7 :rests [:0 :1])
        0.25 2 [:attack 0.1 :release 0.3 :amp 0.5])
   :c (s/phrase-p
       overpad
       (concat (chord-degree :v :C4 :minor) [:3] (chord-degree :i :C4 :minor))
       0.25 0 [:dur 1 :amp 0.5]
       {:refresh 1 :reverse 0.3 :sputter 0.8 :sputter-amt 0.2}
       )
   :d (drum-p [:Kit10-Vinyl :Kit15-Electro] [:p1 :1 :c1])

   :e (drum-p [:Kit10-Vinyl] [:p2 :p3 :2 :p4 :3])
   :toms (s/fit-p {1.75 []} (drum-p [:KurzweilKit08] [:t3 :1 :t4 :t3 :1 :t4 :1]))
   :beat1 (s/fit-p {1.75 []}
                   (drum-p [:KurzweilKit07] [:1 :sd :1 :sd :1]))
   :piano (let [a (chord-degree :iv :C4 :minor)
                b (chord-degree :v :C4 :minor)
                c (chord-degree :vi :C4 :minor)]
            (s/phrase-p
             piano
             [c b a a a a :0 a a a :0 b :3]
             0.25 1 [:dur 1 :hard 0.1 :amp 0.3])
            )
   :shkr (s/fit-p {1.75 []} (drum-p [:Kit8-Vinyl] [:shkr3 :shkr3 :shkr1 :1]))
   :congas (let [p (fn [_]
                 {:phrase (gen-beat (:four-beat @beats)
                                    (map #(vector % [:amp 2])
                                         (concat (vals (drum-kits :Congas))
                                                 (vals (drum-kits :Bongos))
                                                 ))
                                    12
                                    true true 1 0.3 0)
                  :count 0})
             mem (atom (p nil))]
         (fn
           ([] [(s/p-size (get @mem :phrase)) 0.25])
           ([b]
            (let [size (s/p-size (get @mem :phrase))
                  a (get-in @mem [:phrase b])]
              (cond (>= (:count @mem) 2)
                    (swap! mem p)
                    (= size b)
                    (swap! mem (fn [m] (assoc m :count (inc (:count m))))))
              a))
           )
         )
   :main1 (drum-p [:Kit10-Vinyl] [[:k1 :c1] :k4 :p4 :1 :s2 :1 :p3 :5 :s2 :3])
   :main2 (drum-p [:Kit10-Vinyl] [[:k1 :k4] :1 :p1 :k1 :k2 :p1 :k1 :1])
   :main3 (drum-p [:Kit10-Vinyl] [:k1 :1 :c1 :1 :s2 :1 :c1 :1])
   })
(def x-naut
  {:melody (let [a [:Eb5 :G5 :Bb5 :D6 :Bb5 :Eb5]
            b [:Eb5 :G5 :Ab5 :C6 :Ab5 :Eb5]
            c [:Eb5 :Eb5 :F5 :F5 :G5 :G5 :G5 :G5 [:dur 0.2] :1 :G5 :G5 :G5]]
        (s/merge-p
         (s/phrase-p
          sin-inst
          (concat a a a a a
                  b b b b b
                  a a c)
          0.25 0 [:attack 0.1 :release 0.1 :dur 0.2])
         ))
   :harmony (let [a [:D3 :D4]]
                (s/phrase-p
                 piano
                 [a a :6 a a :6]
                 0.25 3 [:dur 2]))
   :bass (let [c [:C2]
            c1 [:C2 [:dur 3]]
            f [:F2]
            f1 [:F2 [:dur 3]]]
        (s/phrase-p
         plk-bass
         (reduce into [c c [:4] c1  c [:4] f f [:4] f1 f [:4]])
         0.25 2))
   }
  )

(def minipops
  {:a (s/phrase-p
       reverb-test
       [:Ab4 :Ab4 :A4 :E4 :E4 :3
        :E4 :E4 :F4 :G4 :G4 :3
        :G4 :G4 :2 :G4 :0 :F4 :0 :D4 :0 :D4 :2
        :D4 :D4 :A4 :E4 :E4]
       0.25 1 [:dur 3 :delay-time 0.4 :decay 5])
   }
  )

(def greenhill
  {:a  (s/phrase-p
        sin-inst
        [:A4 :F4 :A4
         :B4 :G4 :B4
         :C5 :A4 :C5
         :D5 [:dur 0.6]]
        0.25 0 [:dur 0.3])
   :b (s/phrase-p
       bing
       [:C6 :B5 :A5 :G5]
       0.25 1 [:attack 0.05 :decay 0.4 :amp 0.2])
   :c (let [b [:C4 :E4 :B4 [:coef 0.001]]
            a [:C4 :G4 :A4 :F4]
            c [:F4 :A4 :C5]
            d [:D4 :E4 :A4]]
        (s/phrase-p
         ks1
         [b a b a b :2 a :2 c b a]
         0.25 4 [:coef 0.08 :amp 0.4]))
   }
  )

(def tekno
  {:a (let [n :F3]
        (s/phrase-p
         plk-bass
         [n n n n n :1 n n]
         0.25 0 [:amp 0.7 :t 1.5]))
   :b (drum-p
       [:Kit3-Acoustic]
       [:k :3 [kick [:sustain 1 :noise 1 :amp 0.5] :sdst] :3]
       0.25)
   :c (s/phrase-p
       overpad
       [:C4 :Ab4 :G4 :Ab4 [:release 3] :25 :C4 :G4 :F4 :G4 [:release 3] :25
        :C4 :Bb4 :A4 :Bb4 [:release 3] :25]
       0.25 1 [:amp 0.4])
   :d (let [p (fn [_]
                    {:phrase (s/phrase-p
                              bpfsaw
                              (vec (chord-degree (choose [:i :ii :v :iv]) :C3 :minor 4))
                              0.25 0 [:coef 0.01 :dur 0.4 :amp 1 :rq 0.3])
                     :count 0})
                mem (atom (p nil))]
            (fn
              ([] [1.75 0.25])
              ([b]
               (let [a (get-in @mem [:phrase b])]
                 (if (or (>= (:count @mem) 16) ;(= (rand-int 10) 3)
                         )
                   (swap! mem p)
                   (swap! mem (fn [m] (assoc m :count (inc (:count m)))))
                   )
                 a))
              )
            )
   }
  )

(def song-of-storms
  {:a
   (s/phrase-p
    bpfsaw
    [:B3 [:D4 :F4] [:D4 :F4] :C4 [:E4 [:dur 1.5] :G4 [:dur 1.5]] :4
     :D4 [:F4 :A4] [:F4 :A4] :C4 [:E4 [:dur 1.5] :G4 [:dur 1.5]] :4]
    0.25
    1
    [:amp 1 :attack 0.5 :release 1 :atk 0.01])
   :b (let [l [:dur 0.4]]
        (s/phrase-p
         flute
         [:D5 :F5 :D6 :3 :D5 :F5 :D6 :3
          :E6 :1 :F6 l :E6 l :F6 l :E6 l :C6 l :A5 :3
          :A5 :1 :D5 :1 :F5 :G5 :A5 [:dur 0.7] :2
          :A5 :1 :D5 :1 :F5 :G5 :E5 [:dur 0.8] :4
          ]
         0.25
         0
         [:amp 0.2 :dur 0.6 :attack 0.01]))
   })

(def pings
                                        ;(/ 80 60
  {:a (let [root :D3
            notes (scale root :major)]
        (s/phrase-p
         zap
         (map #(midi->hz  (nth notes (degree->int %))) [:ii :iv :v :iii])
         0.25 0 [:freq2 (midi->hz (note root)) :dur 0.2]))
   :b (s/phrase-p
       bing
       [:F#3 :C#5 :B4 :3]
       0.25 1 [:decay 1])
   :c (s/phrase-p
       bpfsaw
       [:Ab4 [:dur 2] :4 :Bb4 :G4 [:dur 1.5] :3]
                                        ;[:B4 [:dur 2] :4 :C#5 :Bb4 [:dur 1.5] :3]
       0.25 2 [:dur 1 :atk 0.01 :amp 0.8]
                                        ;{:refresh 0.3 :reverse 1}
       )
   :d (s/phrase-p
       reverb-test
       [:B4 [:dur 2] :4 :Eb5 :Bb4 [:dur 1.5] :3]
                                        ;[:B4 [:dur 2] :4 :C#5 :Bb4 [:dur 1.5] :3]
       0.25 2 [:decay-time 3 :delay-time 0.5 :amp 0.6])
   :e (fn [b]
        (if (or (= (mod b 3) 0) false)
          (s/chord-p piano (chord-degree (choose [:ii :iii :v :i]) :D4 :major 3) [:amp 0.5])
          )
        )
   :f (s/phrase-p
       klang-test
       [:Eb4 :E4 :Ab4 :Bb4 :0 :B4 :3 :Eb5]
       0.25 2 [:atk 0.01] ;{:refresh 0.5 :reverse 0.3 :sputter 0.5 :sputter-amt 0.6}
       )
   }
  )
(def house
  {:a (s/phrase-p
       rise-pad
       [[:C4 :E4 :G4 :B4] :14
        [:C4 :Eb4 :G4 :A4] :14] 0.25 0 [:detune 0])
   :b (let [a ["SdSt-01"] b ["SdSt-02"] c ["SdSt-05"]]
        (s/m-phrase
         {:refresh 0.5 :sputter 0.7 :sputter-amt 0.2}
         (build-from-kits
          [:Kit3-Acoustic]
          [a :2 a :2 a :1 b c b :2]
          0.125) 0.125))
   :c (let [k ["Kick-02"] k2 ["Kick-01"]]
        (build-from-kits
         [:Kit3-Acoustic]
         [k2 :2 k :2]
         0.25))
   :d (s/phrase-p
       reverb-test
       [:B4 :4 :D5 :2 :C5 :B4 :3 :A4 :4]
       0.25 0 [:amp 1 :decay 1.3 :delay-time 0.6]
       {:refresh 0.4 :reverse 0.3 :sputter 0.5 :sputter-amt 0.1})
   })

(def ambient
  {:a (s/phrase-p
       bing
       [(map #(+ (note :Eb4
                       ) %) (take 4 (range 1 1000 7))) :3
        (map #(+ (note :C4) %) (take 4 (range 1 1000 5))) :3
        (map #(+ (note :D4) %) (take 4 (range 1 1000 5))) :3
        ]
       0.25 0 [:decay 2 :amp 0.2])
   :b (let [a [:D5 :E4] b [:C#5 :Eb4]]
       (s/phrase-p
        sweet
        [a a a b b b]
        0.25 3 [:vib 0 :dur 1 :amp 0.3 :t 1]))
   :c (fn [b]
                 (let [rand #(.nextDouble (ThreadLocalRandom/current) %1 %2)
                       chord (map midi->hz (chord-degree (choose [:i :ii :iii :iv :v]) :E4 :major))]
                   (if (and (integer? b) (= (rand-int 2) 0))
                       (s/chord-p bpfsaw2
                                  chord
                                  [:dur (rand 1.5 4.0)
                                   :detune (rand 0.05 0.1)
                                   :cfmin 100
                                   :cfmax 1500
                                   :rqmin (rand 0.01 0.15)
                                   :atk (rand 2.0 2.5)
                                   :rel (rand 6.5 10.0)
                                   :ldb 6
                                   :amp 1.3])
                       ))
                 )
   })
(def strings
  {:motif (let [a [:C3 :C4] b [:F4 :E3] c [:F3 :E4] d [:F3 :G4] e [:F3 :A4]
         ab (concat a b) ac (concat a c)
         ad (concat a d) ae (concat a e)]
       (s/phrase-p
        ks1
        [ab ac ab ac ab ad ae :8]
        0.25 4 [:coef 0.01 :dur 6]))
   :harmony (s/phrase-p
             flute
             [[:C3 :A4] [:C4 :G3] :12]
             0.25 12 [:dur 5 :amp 0.1])
   }
  )


(def house2
  {:harmony (s/phrase-p
             rise-fall-pad
             [(map midi->hz (chord :C3 :M7)) (map midi->hz (chord :F3 :M7)) :34]
             0.25 32 [:t 5])
   :motif (s/phrase-p
           bass-synth
           [[:E3 :B3] [:A3 :F3] :34]
           0.25 32 [:release 6 :amp 0.4 :detune 4])
   :bass (fn ([] [17.75 0.25])
           ([k]
            (cond
              ;; (or (and (= (mod k (int k)) 0.75) (odd? (int k)))
              ;;     (and (integer? k) (even? k)))
              ;; nil
              true (let [n (note (cond (< k 9.25) :E3
                                       (< k 13.75) :D3
                                       true :F3))
                         action (vector plk-bass [:note (note n) :amp 0.6]
                                        wire-bass [:amp 0.4 :dur 2 :coef 0.01 :decay 0.8])]
                  action))))
   :clap (s/build-map-p
          [:4 [(drum-s [:Kit15-Electro] :cl1) [] o-clap []] :3]
          0.25)
   :motif2 (s/phrase-p
           bass-synth
           [[:E4 :B3] [:A4 [:amp 0.3] :F4 [:amp 0.3]] :34]
           0.25 32 [:release 6 :amp 0.4 :detune 4])
   :motif3 (s/phrase-p
            reverb-test
            [:A4 :B4 :D4 :C4 :B4]
            0.25 2)
   :kick (drum-p
          [:Kit4-Electro :Kit3-Acoustic]
          [:k1 :1 :o1 [:amp 0.3] :1]
          0.25)
   :cl  (drum-pattern
           [:Kit16-Electro]
           [[c1] [c1] [c2] :2 [c1] :2]
           0.25 [:amp 0.3])
   :snr (drum-pattern
         [:Kit3-Acoustic :Kit16-Electro :Kit4-Electro]
         [:2 [s3] :1]
         0.25)
   :t (drum-pattern
       [:Kit5-Electro]
       [[cl1] :2]
       0.25)
   ;; :test (let [a (doall (map kill (node-tree-matching-synth-ids "test" 1)))
   ;;             b  (doall (map kill (node-tree-matching-synth-ids "test" 0)))
   ;;             b (node "test" {:freq (midi->hz (note :B4))})]
   ;;         (on-event [:midi :note-on]
   ;;                   (fn [m]
   ;;                     (let [root "Eb"
   ;;                           cur-scale (map find-pitch-class-name
   ;;                                          (scale (keyword (str root "4")) :major))
   ;;                           info (note-info (find-note-name (:data1 m)))]
   ;;                       (ctl b :freq (midi->hz (:midi-note info)))
   ;;                       ))
   ;;                   :test-midi)
   ;;         {1 []})
   })

(def ambient2
  {:motif (let [p (fn [_]
                    {:phrase (s/phrase-p
                              bpfsaw
                              (vec (chord-degree (choose [:i :v :iv]) :A3 :minor 4))
                              0.25 0 [:coef 0.01 :dur 0.5 :amp 1])
                     :count 0})
                mem (atom (p nil))]
            (fn
              ([] [1.75 0.25])
              ([b]
               (let [a (get-in @mem [:phrase b])]
                 (if (or (>= (:count @mem) 16) ;(= (rand-int 10) 3)
                         )
                   (swap! mem p)
                   (swap! mem (fn [m] (assoc m :count (inc (:count m)))))
                   )
                 a))
              )
            )
   :bells (s/fit-p {1.75 []}
                   (s/phrase-p
                    reverb-test
                    ;bing
                    (gen-phrase (concat (scale :C4 :major) (scale :C5 :major)) 12 :degree 7 :rests [:0 :1 :2])
                    0.25 2 [:attack 0.1 :release 0.3 :amp 0.5 :coef 0.001 :dur 1 :decay 1]))
   :piano (let [a [:G4 :B4]
                 b [:G4 :C5]]
             (s/phrase-p
              piano
              [a a a a a :1 b :1 a :1 b :1 a :3]
              0.25 2 [:attack 0.1 :release 0.3 :hard 0.3 :amp 0.3 :coef 0.001 :dur 1 :decay 1]))
   :harmony (let [mk-prog (fn [p]
                            (conj
                             (vec
                              (mapcat #(vector (chord-degree % :C4 :major)) p)) :28))
                  a (mk-prog [:ii :iii :i])
                  b (mk-prog [:iv :iii :i])]
              (s/fit-p
               {1.75 []}
               (s/phrase-p
                rise-pad
                a
                ;; flute
                ;; b
                0.25 28 [:t 6 :amp 0.3 :dur 6 :attack 0.3 :sustain 0.2])))
   :bass (fn
           ([] [12.75 0.25])
           ([b]
            (if (= b 1)
              (s/chord-p
               bass-synth
                                        ;(chord-degree (choose [:i :vi :iii :iv]) :C4 :major 4)
               (map midi->hz (chord-degree (choose [:i :ii :iii :vi]) :C3 :major 4))
               [:coef 0.01 :t 10 :attack 8 :release 5 :amp 0.07])
              )))
   :clap (drum-p
          [:Kit5-Electro]
          [:4 :cl :3]
          0.25 [:amp 1.5])
   :crash (drum-p [:Kit3-Acoustic] [:cr3 :cr3 :cr3 :cr3])
   :kick (drum-p [:Kit4-Electro] [:k2 :3])
   :tick (let [t [bing [(note :C5) 0.001 0.1 1.5]]]
           (drum-p [:Kit4-Electro] [t :2 t :1]))
   :marimba (fn [b]
           (let [cfmin  (choose (map midi->hz (scale :C5 :major)))
                ; cfmin (* (midi->hz 64) (choose [0.5 1 2 4]))
                 ]
             (if (= (rand-int 2) 1)
               [bpfsaw2
                [:dur (choose [1 0.5])
                 :freq (choose (map double [(/ 1 2) (/ 2 3) 1 (/ 4 3) 2 (/ 5 2) 3 4 6 8]))
                 :detune (rand 0.1)
                 :rqmin 0.005
                 :rqmax 0.008
                 :cfmin cfmin
                 :cfmax (* cfmin (choose (range 1.008 1.025 0.001)))
                 :atk 3
                 :sus 1
                 :rel 5
                 :amp 5]]))
              )
   :claves(let [p (fn [_]
                 {:phrase (gen-beat (:four-beat @beats)
                                    (map #(vector % [:amp 1.3]) (concat (vals (drum-kits :claves))
                                                                        ))
                                    12
                                    true true 1 0.3 0)
                  :count 0})
             mem (atom (p nil))]
         (fn
           ([] [(s/p-size (get @mem :phrase)) 0.25])
           ([b]
            (let [size (s/p-size (get @mem :phrase))
                  a (get-in @mem [:phrase b])]
              (cond (>= (:count @mem) 2)
                    (swap! mem p)
                    (= size b)
                    (swap! mem (fn [m] (assoc m :count (inc (:count m))))))
              a))
           )
         )
   })


(def synth-pop
  {:drum1 (s/build-map-p
           [:5 [ (get-in drum-kits [:Kit7-Electro :CYCdh_ElecK04-Kick01.wav])[]] :2] 0.125)
   :drum2 (s/build-map-p
           [:1 [ (get-in drum-kits [:Kit7-Electro :CYCdh_ElecK04-Cymbal01.wav])[]] :6] 0.125)
   :drum3 (s/build-map-p
            [:15 [ (get-in drum-kits [:Kit4-Electro :CYCdh_ElecK01-Kick02.wav])[]]
             :13 [ (get-in drum-kits [:Kit4-Electro :CYCdh_ElecK01-Snr02.wav])[]]
             :1 [ (get-in drum-kits [:Kit4-Electro :CYCdh_ElecK01-Kick02.wav])[]]
             :3 [ (get-in drum-kits [:Kit4-Electro :CYCdh_ElecK01-Snr03.wav])[]]:12] 0.125)
   :melody1 (s/build-map-p
             [[ bpfsaw[:decay 2 :dur 1 :atk 0.01 :note (note :C4)]]
              :1 [ bpfsaw[:decay 2 :dur 1 :atk 0.01 :note (note :D4)]]
              :3 [ bpfsaw[:decay 2 :dur 1 :atk 0.01 :note (note :A4)]] :1] 0.125)

   })

(def house3
  {:kick (s/build-map-p
          [[(drum-s [:Kit18-Acoustic] :k1) [:amp 0.4]] :3]
          )
   :hat (drum-p [:Kit1-Acousticclose :Kit5-Electro]
                [:2 :c5 :3 [:c5 :cl1] :3 :c5 :3
                 [:c5 :cl1] :3 :c5 :c5 :2 [:c5 :cl1] :1]
                0.25 [:amp 0.5])
   :bass (s/phrase-p
          acid-bass
          [:G1 :Bb1 :G1 :Bb1 :G1 :3 :G1 :Bb1 :G1 :Bb1 :G1 :3]
          ;[:G1 :Bb1 :G1 :Bb1 :A1 :3 :G1 :Bb1 :G1 :Bb1 :A1 :3]
          0.25 2 [:amp 0.05])
   :perc (drum-p [:KurzweilKit08]
                   [:p3 :2 :p3 :2 :p3 :2 :p3 :3 :p2 :1 :p3 ]
                   )
   :harmony (let [amp 0.09
                  a [:D5 :F5]
                  b [:A5 :C6]
                  c [:C5 :E5]
                  d [:G5 :B5]
                  b2 [:A5 [:amp amp :atk 0.01 :dur 4] :C6 [:amp amp :atk 0.01 :dur 4]]
                  d2 [:G5 [:amp amp :atk 0.01 :dur 4] :B5 [:amp amp :atk 0.01 :dur 4]]]
              (s/phrase-p
               klang-test
               [a b :4 a b2 :4  a :1 b :1 a :1 b :1 a :1 b :5
                c d :4 c d2 :4  c :1 d :1 c :1 d :1 c :1 d :5]
               ;; [a b :8 a b2 :9  a :3 b :3 a :3 b :3 a :3 b :6
               ;;  c d :8 c d2 :9  c :3 d :3 c :3 d :3 c :3 d :6]
               0.25 2 [:amp amp :atk 0.01]))
   :flute (s/phrase-p
           flute
           [:A5 :14 :B5 :16 :D6 :16]
           0.25 0 [:dur 2.4 :amp 0.15])
   :clap (drum-p [:Kit15-Electro]
                 [:4 :cl1 [:amp 0.5] :3])
   :click (drum-p
           [:KurzweilKit07]
           [:c1 :c1]
           0.25 0 [:amp 0.3])
   :sdst (drum-p
          [:KurzweilKit05]
          [:sd :2]
          0.25)
   :bass2 (s/phrase-p
           bass2
           [:A4 :B4 :C4 :D4 :D4 :1 :D4 :1]
           0.25 2 [:decay 2])
   :bass3 (s/phrase-p
           bass2
           [:E5 :D5 :C5 :B4 :6]
           0.25 2 [:decay 2 :cutoff2 4000])
   })
(def chill
  {:drum1 (s/build-map-p
           [[o-kick []] :1 [o-hat []] :2 [o-hat []] :2]
           0.25 0)
   :drum2 (s/build-map-p
           [[(drum-s [:KurzweilKit03] :c1) []] :3]
           0.25 0)
   :drum3 (s/build-map-p
           [:5 [o-clap []
                (drum-s [:Kit15-Electro] :cl1) [:amp 2]
                ]]
           0.25 0)
   :harmony1 (s/phrase-p
              bpfsaw
              [(chord :B3 :m7) :6 (chord :A3 :m7) :8]
              0.25 0 [:rq 0.6 :dur 1.6 :amp 0.2 :atk 0.7])
   :motif1 (s/phrase-p
            acid-bass
            [:F1 :1 :B1 :2 :G1 :2 :A1 :3]
            0.25 0 [:dur 0.3])
   :motif2 (s/phrase-p
            bass2
            [:D4 :1 :G4 :2 :C4 :2 :A4 :3]
            0.25 0 [:dur 0.3 :decay 3 :amp 1.5])
   })

(def exp1
  {:harmony (s/phrase-p
             bpfsaw
             [[:B3 :F#4] :7 [:E4 :A3] :7 [:D4 :F#3] :7 [:F#3 :C#4] :7]
             0.25 0 [:dur 2.204 :detune 0.0 :rq 0.897])
   :motif (s/phrase-p
           bass2
           [[:C#5 :Ab5 :D6] :E6 [:D6 :Ab5 :C#5 :C#6] :2 [:Ab5 :D6 :C#5] :E6 :D6 :2]
           0.25 0 [:cutoff 5354.33 :decay 3.40 :amp 2.51])
   :drum (s/build-map-p
          [[ o-kick []   ] :2
           [ o-kick []] [o-snr []] :3
           [ o-kick []   ] :2
           [ o-kick []   ] [o-snr []] :3
           [ o-kick []   ] :2
           [ o-kick []   ] [o-snr []] :3
           ])

   })
(def exp2
  {:hat (drum-p
         [:Kit16-Electro]
         [:c1 :2 :c1 :2 :c1 :2 :c1 :2 :c1 :1 :c1 :c2]
         0.25 [:amp 0.7])
   :bass (s/phrase-p
          bass2
          [:E2 :E2 :E2 :E2 :1 :E2 :1 :G2 [:atk 0.001 :decay 2 :cutoff2 1500.92 :amp 0.7] :2]
          0.25 2 [:atk 0.001 :decay 1.2 :cutoff2 1000.92 :amp 0.7])

   :kick (drum-p
          [:Kit16-Electro]
          [[o-kick [:amp 0.4]] :3])
   :clap (drum-p
          [:Kit15-Electro]
          [:4 :cl1 :3])
   :clap2 (drum-p
           [:Kit15-Electro]
           [:4 :cl1 :7 :cl1 :7 :cl1 :7 :cl1 :cl1 :cl1 :1])
   :motif (s/phrase-p
           bass2
           [:B4 :F5 :D5 :E5 :C5 :1 :B4 :1]
           0.25 2 [:cutoff2 3362.20 :atk 0.0012 :decay 3.02 :amp 0.5])
   :prophet (s/phrase-p
             prophet
             [:D3 :14 :C3 :14 :B2 :14 :A2 [:attack 1.81 :decay 4 :cutoff 2125.984 :amp 0.3] :18]
             0.25 0 [:attack 1.81 :decay 2.2 :cutoff 2125.984 :amp 0.3])
   :rise (s/phrase-p
          rise-pad
          [[:D5 :D5]
           [:D5 :D5] :3
           [:C5 :C5] :C5
           [:C5 :C5]
           [:C5 :C5]
           [:C5 :C5] :C5
           [:B4 :B4] :B4
           [:B4 :B4]
           [:B4 :B4] :B4 :B4
           [:A4 :A4]
           [:A4 :A4]
           [:A4 :A4] [:A4 :A4] :A4]
          0.25 2 [])
   :bass2 (s/phrase-p
           acid-bass
           [:C2 :C2 :1 :D2 :1 :C2 :D2 :1]
           0.25 0 [:dur 0.47 :amp 0.2])
   :beat1 (s/build-map-p
           [[ b-kick []   ] :1
            [ b-kick []   ] :4
            [ o-snr [] (get-in drum-kits [:Kit16-Electro :CYCdh_ElecK06-Snr01.wav]) []   ] :2
            [ o-snr [] (get-in drum-kits [:Kit16-Electro :CYCdh_ElecK06-Snr01.wav]) []   ] :4
            [ b-kick []   ]
            ])
   :motif2 (s/phrase-p
            bass-synth
            [:E4 :3 :E4 :F4 :E4 :F4 :E4 :F4 :E4 :4 :F4 :G4
             :E4 :D4 :E4 :2]
            0.25 1 [:attack 0.03 :release 1 :detune 0 :bwr 2 :amp 0.7])
   :rise2 (s/phrase-p
           rise-pad
           [[:B4 :F5]
            [:B4 :F5]
            [:A4 :E5]
            [:A4 :E5]
            [:G4 :E5]
            [:G4 :E5]
            [:A4 :E5]
            [:A4 :E5] :6]
           0.25 6 [:t 2])
   })

(def exp3
  {:beat1 (s/build-map-p
           [[o-kick []] :1
            [b-kick []]
            [o-kick []] [o-kick []] :1
            [o-clap [] o-snr []] :1
            [o-kick []] :1
            [b-kick []]
            [o-kick []] [o-kick []]
            [o-snr [:amp 0.5] (drum-s [:Kit15-Electro] :cl1) [:amp 2]] :2]
           0.25)
   :kick (s/build-map-p
          [[o-kick []] :3  ]
          0.25)
   :beat2 (s/build-map-p
           [:1
            [ (drum-s [:Kit15-Electro] :cl2) []]
            [ (drum-s [:Kit15-Electro] :cl2) []   ]
            [ (drum-s [:Kit15-Electro] :cl1) []   ] :2
            [ (drum-s [:Kit15-Electro] :cl1) []   ]
            [ (drum-s [:Kit15-Electro] :cl2) []   ]
            [ (drum-s [:Kit15-Electro] :cl1) []   ] :1
            [ (drum-s [:Kit15-Electro] :cl1) []   ]
            [ (drum-s [:Kit15-Electro] :cl2) []   ]
            [ (drum-s [:Kit15-Electro] :cl1) []   ]
            [ o-snr []   ] :2])
   :harmony (s/phrase-p
             prophet
             [[:C#4 :F#3] [:C#4 :F#3]
              [:C#4 :F#3] [:F#3 :C#4]
              [:C#4 :F#3]
                                        ;[:C#4 :F#3] [:F#3 :C#4] [:C#4 :F#3] [:F#3 :C#4]
              :3
              [:A3 :D3] [:A3 :D3] [:A3 :D3] [:A3 :D3] [:A3 :D3] :3
              [:B3 :E3] [:B3 :E3]  [:B3 :E3] [:B3 :E3]
              [:B3 :E3] :3]
             0.25 2 [:attack 0.001 :decay 1.3 :amp 0.5 :cutoff 2000])
   :harmony2 (s/phrase-p
              bpfsaw2
              [[:A5 :A5]
               [:A5 :A5] :A5 :2 :Ab5 [:Ab5 :Ab5] [:Ab5 :Ab5] [:Ab5 :Ab5] :Ab5 :1 :F#5 [:F#5 :F#5] :F#5 :F#5 :5 [:D5 :D5] :D5 :D5 :C#5 [:C#5 :C#5] [:C#5 :C#5] :C#5 [:rel 3 :sus 1] :24]
              0.25 3 [:rel 2])
   })
