(ns techno.sketches2
  (:import java.util.concurrent.ThreadLocalRandom)
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.player :as p]
        [techno.synths]
        [techno.samples]
        [techno.drum-patterns]
        )
  )


(comment
  (def player (get-s 80))
  (def player 21)
  (stop-s player)
  (p/set-sp player 90)
  (p/rm-p player :hats)
  (let [patterns {:kick (p/drums
                         [:Kit4-Electro :Kit10-Vinyl]
                         [:k1 :|]
                         1/8)
                  :hats (p/drums
                         [:Kit10-Vinyl]
                         [:o1 :| :c1 :c2 :1 :c1 :|
                          :c2 :c1 :c1 :|]
                         1/4)
                  :clap (p/drums
                         [:Kit15-Electro]
                         [:cl1 :| :|]
                         1/4)
                  :bass (p/phrase-p
                         acid-bass
                         [:C2 :3 :A1 :1 :B1 :| :| :|]
                         1/8 0 [:dur 0.4])
                  }]
    (p/rm-p player :all)
    (doseq [[k p] patterns]
      (p/add-p player p k)
      )
    )

  )

(def template
  {:drum1 (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            (drum-p2
             [:Kit15-Electro]
             []
             1/4))
   :drum2 (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            (drum-p2
             [:Kit15-Electro]
             []
             1/4))
   :drum3 (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            (drum-p2
             [:Kit15-Electro]
             []
             1/4))
   :drum4 (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            (drum-p2
             [:Kit15-Electro]
             []
             1/4))
   :drum5 (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            (drum-p2
             [:Kit15-Electro]
             []
             1/4))
   :drum6 (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            (drum-p2
             [:Kit15-Electro]
             []
             1/4))
   :drum7 (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            (drum-p2
             [:Kit15-Electro]
             []
             1/4))
   :drum8 (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            (drum-p2
             [:Kit15-Electro]
             []
             1/4))
   :motif1 (p/phrase-p
            bpfsaw
            []
            0.25 0 [])
   :motif2 (p/phrase-p
            overpad
            []
            0.25 0 [])
   :motif3 (p/phrase-p
            bass2
            []
            0.25 0 [])
   :motif4 (p/phrase-p
            plk-bass
            []
            0.25 0 [])
   :motif5 (p/phrase-p
            bpfsaw
            []
            0.25 0 [])
   :harmony1 (p/phrase-p
            bpfsaw
            []
            0.25 0 [])
   :harmony2 (p/phrase-p
            overpad
            []
            0.25 0 [])
   :harmony3 (p/phrase-p
            bass2
            []
            0.25 0 [])
   :harmony4 (p/phrase-p
            plk-bass
            []
            0.25 0 [])
   :harmony5 (p/phrase-p
            bpfsaw
            []
            0.25 0 [])
   :metronome (p/phrase-p
               bing
               [:C4 :|]
               1/4)
   }
  )

(def sketch
  {:tabla  (let []
             (drum-p2
              [:tabla]
              [:dhin  :ge :| :ge :1 :te :tin :|])
             )
   :rise   (p/phrase-p
            rise-pad
            [[:G4 :F3 :D4] :12 [:E4 :B4 :G3] :12 [:C4 :A3 :G4 :D3] :| :| :| :| :| :|]
            1/4 0 [:t 4])
   :bing (p/phrase-p
          bing
          [:C3 :1 :G3 :1 :D4 :1 :B4 :| :|]
          1/8 0 [:decay 1.8])
   :bell   (p/phrase-p
            reverb-test
            [:A3 :8 :D4 :8 :G4 :| :| :| :|]
            1/4 0 [:decay 5.354330708661418])
   :bass  (p/phrase-p
           prophet
           [:A2 :| :| :D3 :| :| :G3 :| :| :D3 :| :| :A2 :| :| :| :|]
           1/4 0 [:t 5 :attack 1 :release 4 :bwr 0.2 :cutoff 400])

   })

(def bells
  {:klang (p/phrase-p
            klang-test
            [:G3  :F#4  :F#4  :G3  :F#4  :F#4  :G3  :F#4  :A3  :G4  :G4  :A3  :G4  :G4  :A3  :G4  :B3  :B4  :B4  :B3  :B4  :B4  :B3  :B4  :C#4  :A4  :A4  :C#4  :A4  :A4
             :C#4  :A4 :1]
            1/4 1 [:atk 0.01 :dur 1.6])
   :sin (p/phrase-p
            sin-inst
            [:G4  :D5  :G4 :D5  :G4  :D5  :G4  :D5  :A4  :E5  :A4  :E5  :A4  :E5  :A4  :E5  :F#4  :E5  :F#4  :E5  :F#4  :E5  :F#4  :E5  :F#4  :D5  :F#4 :D5  :F#4 :D5  :F#4  :D5 :|]
            1/4 1 [:dur 0.7 :amp 0.5])
   :bpfsaw (p/phrase-p
            bpfsaw
            [:G4 :B4  :A5  :G4  :B4  :G5 :G4 :B4  :F#5  :G4  :B4  :E5  :F#4  :A4  :D5 :|]
            1/4 1 [:dur 1.25 :atk 0.01])
   :flute (p/phrase-p
           flute
           [:G4 :G4 :G4 :12 :E5 :E5 :4 :D5 :D5 :8 :A4 :1 :A4 :1 :A4 :10 :B4 :1 :B4 :B4 :8]
           1/4 0 [])
   :bowed (p/phrase-p
           bowed
           [[:A3 :D4 :A4] :A3 [:A4 :D4] :1 [:A3 :A4] :1 :A3 :A3 [:A4 :D4] [:A3 :D4] :1 :A3 :D4 :1 [:D4 :A4 :A3] :1 [:D4 :A3 :A4] :1 [:A3 :D4 :A4] :1 [:A3 :D4 :A4] :1 [:A3 :D4 :A4] :1 [:A3 :D4 :A4] :A3 :D4 [:A3 :D4 :A4] :3 [:G4 :C4 :G3] :1 [:G3 :C4 :G4] :1 [:G4 :G3 :C4] :1 [:C4 :G4 :G3] :1 [:C4 :G4 :G3] :1 [:C4 :G3 :G4] :1 [:G3 :C4 :G4] :1 [:C4 :G3 :G4] :1 [:C4 :G3 :G4] :1 [:G4 :C4] :G3 [:C4 :G4 :G3] :1 [:C4 :G4 :G3] :1 [:C4 :G3 :G4] :1 [:C4 :G3] :1 [:G4 :C4 :G3] :3 [:A3 :D3] :1 [:A3 :D3] :A3 :D3 [:A3 :D3] :1 :A3 :1 [:A3 :D3] :1 [:A3 :D3] :A3 :D3 :A3 :D3 :A3 :D3 [:A3 [:dur 1.5] :D3 [:dur 1.5]] :1 [:A3 [:dur 5.5] :D3 [:dur 5.5]] :1 [:A3 [:dur 7] :D3 [:dur 7]] :42]
           1/4 0 [:amp 0.07])
   })

(def exp3
  {:beat1 (p/build-map-p
           [[o-kick []] :1
            [b-kick []]
            [o-kick []] [o-kick []] :1
            [o-clap [] o-snr []] :1
            [o-kick []] :1
            [b-kick []]
            [o-kick []] [o-kick []]
            [o-snr [:amp 0.5] (drum-s [:Kit15-Electro] :cl1) [:amp 2]] :2]
           0.25)
   :kick (p/build-map-p
          [[o-kick []] :3  ]
          0.25)
   :beat2 (p/build-map-p
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
   :harmony (p/phrase-p
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
   :arp (let [a [:D4 :F#4  :A4]
              b [:C#4 :E4  :Ab4]
              c [:C#4  :F#3 :A3]]
          (p/phrase-p
           bpfsaw
           (flatten (concat (repeat 10 a) (repeat 6 b) (repeat 16 c)))
           0.25 0 [:dur 0.7 :atk 0.1 :rq 0.6]))
   :harmony2 (p/phrase-p
              bpfsaw2
              [[:A5 :A5]
               [:A5 :A5] :A5 :2 :Ab5 [:Ab5 :Ab5] [:Ab5 :Ab5] [:Ab5 :Ab5] :Ab5 :1 :F#5 [:F#5 :F#5] :F#5 :F#5 :5 [:D5 :D5] :D5 :D5 :C#5 [:C#5 :C#5] [:C#5 :C#5] :C#5 [:rel 3 :sus 1] :24]
              0.25 3 [:rel 2])
   :clap (drum-p2
          [:Kit15-Electro]
          [:4 :cl1 :3] 0.25)
   :motif (let [a [:E4 :1 :C#5 :2 :C#5 :2 :C#5 :2 :C#5 :1 :C#5 :2]
                b [:F#4 :1 :C#5 :2 :C#5 :2 :C#5 :2 :C#5 :1 :C#5 :2]]
            (p/phrase-p
             bass2
             (flatten (concat (repeat 6 a) (repeat 6 b) ))
             0.25 0 [:decay 2.4 :amp 2.4 :cutoff 6362 :cutoff2 3362]))
   })
(def chill
  {:drum1 (p/build-map-p
           [[o-kick []] :1 [o-hat []] :2 [o-hat []] :2]
           1/4 0)
   :drum2 (p/build-map-p
           [[(drum-s [:KurzweilKit03] :c1) []] :3]
           1/4 0)
   :drum3 (p/build-map-p
           [:5 [o-clap []
                (drum-s [:Kit15-Electro] :cl1) [:amp 2]
                ]]
           1/4 0)
   :harmony1 (p/phrase-p
              bpfsaw
              [(chord :B3 :m7) :6 (chord :A3 :m7) :8]
              1/4 0 [:rq 0.6 :dur 1.6 :amp 0.2 :atk 0.7])
   :motif1 (p/phrase-p
            acid-bass
            [:F1 :1 :B1 :2 :G1 :2 :A1 :3]
            1/4 0 [:dur 0.3])
   :motif2 (p/phrase-p
            bass2
            [:D4 :1 :G4 :2 :C4 :2 :A4 :3]
            1/4 0 [:dur 0.3 :decay 3 :amp 1.5])
   })

(def wavy
  {:drone (p/phrase-p
           bpfsaw2
           [:A4 :2 :A4 :3 :A4 :| :3 :A4 :2 :A4 :| :2 :A4 :3 :A4 :| :2 :A4 :3 :A4 :| :2 :A4 :2 :A4 :| :| :5 :D5 :| :1 :D5 :2 :D5 :| :D5 :2 :D5 :3 :D5 :| :2 :D5 :3 :D5 :| :| :| :3 :B4 :2 :B4 :| :2 :B4 :3 :B4 :| :3 :B4 :3 :B4 :| :3 :B4 :2 :B4 :| :3 :B4 :3 :B4 :| :2 :B4 :4 :A4 :| :3 :A4 :3 :A4 :| :3 :A4 :3 :A4 :| :| :| :| :| :3 :F4 :3 :F4 :| :3 :F4 :2 :F4 :| :2 :F4 :3 :F4 :| :1 :F4 :4 :F4 :| :3 :G4 :3 :G4 :| :2 :G4 :3 :G4 :| :2 :G4 :3 :G4 :| :2 :G4 :| :2 :E4 :4 :E4 :| :3 :E4 :3 :E4 :| :3 :E4 :3 :E4 :| :3 :E4 :| :3 :D4 :3 :D4 :| :3 :D4 :3 :D4 :| :3 :D4 :| :|]
           1/8 0 [])
   :hats (let [o o-kick
               s o-snr
               b b-kick
               bs b-snr
               c o-clap
               h o-hat
               d dirty-kick
               g g-kick
               r r-kick]
           (drum-p2
            [:Kit11-Vinyl]
            [:c1 :c1 :c2 :2 :| :c2 :c1 :2 :c1 :|
             :4 :c1 :1 :c2 :| :c2 :c1 :2 :c1 :|]
            1/8))
   :kicks (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            (drum-p2
             [:Kit12-Vinyl]
             [:k1 :2 :k1  :| :k2 :|
              :k1  :1 :c1 :| :k1 :c1  :|
              ]
             1/4))
   :bass (p/phrase-p
          prophet
          [:A2 :2 :A2 :| :2 :A2 :| :D3 :2 :D3 :| :2 :D3 :| :Bb2 :2 :Bb2 :| :2 :Bb2 :| :B2 :2 :B2 :| :2 :B2     :|]
 1/4 0 [:decay 1 :attack -0.07874015748031496])
   :klang (p/phrase-p
           klang-test
           [:D5 :3 :D6 :| :A5 :| :| :7 :F5 :| :5 :F5 :| :3 :A5 :| :2 :E5 :3 :A5 :| :| :D5 :3 :D6 :| :A5 :| :| :7 :E5 :| :5 :E5 :| :3 :A5 :| :2 :G5 :4 :A5 :| :8]
           1/8 0 [:atk -0.007874015865363473])

   })

(def drumaa
  {:drum2 (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            (drum-p2
             [:KurzweilKit07]
             [:k1    :| :c1 :1 :c2 :|
              :2 :k2 :| :c1 :1 :s1 :|]
             1/4))
   :drum3 (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            (drum-p2
             [:KurzweilKit05]
             [:sd :| ;:3 :sd :1 :sd :1 :sd :|
              ]
             1/8))
   :drum4 (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            (drum-p2
             [:KurzweilKit07]
             [:s1 :| :s2 :|]
             1/4))

   })

(def rando
  {:saw (p/phrase-p
         bpfsaw
         [:D5 :3 :D4 :1 :G4 :| :| :4 :C4 :1 :G4 :| :|
          :D5 :3 :F4 :1 :A4 :| :B4 :2 :E4 :| :4 :G4 :1 :E4 :| :|]
         1/8 0 [:atk 0.01 :dur 1.2])
   :cl (let [o o-kick
             s o-snr
             b b-kick
             bs b-snr
             c o-clap
             h o-hat
             d dirty-kick
             g g-kick
             r r-kick]
         (drum-p2
          [:Kit14-Acoustic]
          [:c1 :1 :c2 :| :c2 :c2 :c2 :3 :c1 :|
           :|]
          1/8))
   :bass (p/phrase-p
          bass-synth
          [:G2 :1 :D3 :3 :F2 :| :D3 :|
           :F2 :1 :C3 :3 :F2 :| :A3 :|]
          1/8 0 [:release 1.1 :attack 0.01 :amp 1 :bwr 1.2 :detune 1.4])
   :snr (let [o o-kick
              s o-snr
              b b-kick
              bs b-snr
              c o-clap
              h o-hat
              d dirty-kick
              g g-kick
              r r-kick]
          (drum-p2
           [:KurzweilKit07]
           [:2 :s1 :| :|
            :2 :s1 :| :1 :s1 :s2 :|]
           1/4))
   :kick (let [o o-kick
               s o-snr
               b b-kick
               bs b-snr
               c o-clap
               h o-hat
               d dirty-kick
               g g-kick
               r r-kick]
           (drum-p2
            [:KurzweilKit05]
            [:k1 :| :k1 :k2 :|
             :k2 :| :k1 :|]
            1/4))

   })

(def house3
  {:kick (p/build-map-p
          [[(drum-s [:Kit18-Acoustic] :k1) [:amp 0.4]] :3]
          )
   :hat (drum-p2 [:Kit1-Acousticclose :Kit5-Electro]
                [:2 :c5 :3 [:c5 :cl1] :3 :c5 :3
                 [:c5 :cl1] :3 :c5 :c5 :2 [:c5 :cl1] :1]
                1/4 [:amp 0.5])
   :bass (p/phrase-p
          acid-bass
          [:G1 :Bb1 :G1 :Bb1 :G1 :3 :G1 :Bb1 :G1 :Bb1 :G1 :3]
          ;[:G1 :Bb1 :G1 :Bb1 :A1 :3 :G1 :Bb1 :G1 :Bb1 :A1 :3]
          1/4 2 [:amp 0.05])
   :perc (drum-p2 [:KurzweilKit08]
                   [:p3 :2 :p3 :2 :p3 :2 :p3 :3 :p2 :1 :p3 ]
                   )
   :harmony (let [amp 0.09
                  a [:D5 :F5]
                  b [:A5 :C6]
                  c [:C5 :E5]
                  d [:G5 :B5]
                  b2 [:A5 [:amp amp :atk 0.01 :dur 4] :C6 [:amp amp :atk 0.01 :dur 4]]
                  d2 [:G5 [:amp amp :atk 0.01 :dur 4] :B5 [:amp amp :atk 0.01 :dur 4]]]
              (p/phrase-p
               klang-test
               [a b :4 a b2 :4  a :1 b :1 a :1 b :1 a :1 b :5
                c d :4 c d2 :4  c :1 d :1 c :1 d :1 c :1 d :5]
               ;; [a b :8 a b2 :9  a :3 b :3 a :3 b :3 a :3 b :6
               ;;  c d :8 c d2 :9  c :3 d :3 c :3 d :3 c :3 d :6]
               1/4 2 [:amp amp :atk 0.01]))
   :flute (p/phrase-p
           flute
           [:A5 :14 :B5 :16 :D6 :16]
           1/4 0 [:dur 2.4 :amp 0.15])
   :clap (drum-p2 [:Kit15-Electro]
                 [:4 :cl1 [:amp 0.5] :3])
   :click (drum-p2
           [:KurzweilKit07]
           [:c1 :c1]
           1/4 0 [:amp 0.3])
   :sdst (drum-p2
          [:KurzweilKit05]
          [:sd :2]
          1/4)
   :bass2 (p/phrase-p
           bass2
           [:A4 :B4 :C4 :D4 :D4 :1 :D4 :1]
           1/4 2 [:decay 2])
   :bass3 (p/phrase-p
           bass2
           [:E5 :D5 :C5 :B4 :6]
           1/4 2 [:decay 2 :cutoff2 4000])
   })

(def pop
  {:motif1 (p/phrase-p
            plk-bass
            [:B1 :Eb2 :1 :G2 :| :2 :Bb1 :| :Eb2 :| :|]
            1/8 0 [:amp 2.7559055118110236 :dur 0.5905511811023622])
   :motif2 (p/phrase-p
            bpfsaw
            [:C5 :Eb5 :2 :G5 :2 :Bb5 :| :| :7 :Eb5 :| :| :D5 :| :1 :C5 :| :1 :Bb4 :|
             :G5                     :| :| :| :| :| :|]
            1/8 0 [:atk 0.01 :dur 1.8])
   :harmony1 (p/phrase-p
              overpad
              [[:G4 :C4 :Eb4] :| :5 [:G4 :C4] :| :| :G4 :| :7 [:Bb4 :G4 :D4] :| :7 [:Bb4 :G4 :D4] :| :7 [:Bb3 :D4] :| :| :2 [:G3 :D4 :Bb3] :| :4 [:F3 :Eb4 :C4] :| :| :| :|]
              1/8 0 [:attack 1 :release 1])
   :drum2 (p/build-map-p
           [[ (get-in drum-kits [:KurzweilKit05 :CYCdh_Kurz05-Kicki02.wav]) []   ] :1
            [ (get-in drum-kits [:KurzweilKit05 :CYCdh_Kurz05-Kicki02.wav]) []   ] :|
            [ (get-in drum-kits [:KurzweilKit05 :CYCdh_Kurz05-SdSt.wav]) []   ] :1
            [ (get-in drum-kits [:KurzweilKit05 :CYCdh_Kurz05-Kicki02.wav]) []   ] :|
            [ (get-in drum-kits [:KurzweilKit05 :CYCdh_Kurz05-SdSt.wav]) []   ] :1
            [ (get-in drum-kits [:KurzweilKit05 :CYCdh_Kurz05-SdSt.wav]) []   ] :|
            [ (get-in drum-kits [:KurzweilKit05 :CYCdh_Kurz05-Kicki02.wav]) []   ]
            ] 1/4)
   :kick (p/build-map-p
          [[ (get-in drum-kits [:Kit11-Vinyl :CYCdh_VinylK4-Kick01.wav]) []   ] :1
           [ (get-in drum-kits [:Kit11-Vinyl :CYCdh_VinylK4-Kick02.wav]) []   ] :|
           [ (get-in drum-kits [:Kit11-Vinyl :CYCdh_VinylK4-Kick01.wav]) []   ] :1
           [ (get-in drum-kits [:Kit11-Vinyl :CYCdh_VinylK4-ClHat02.wav]) []   ]
           :|
           :|
           [ (get-in drum-kits [:Kit11-Vinyl :CYCdh_VinylK4-Kick01.wav]) []   ] :1
           [ (get-in drum-kits [:Kit11-Vinyl :CYCdh_VinylK4-Kick02.wav]) []   ] :|
           [ (get-in drum-kits [:Kit11-Vinyl :CYCdh_VinylK4-Kick01.wav]) []   ] :1
           :|
           ] 1/4)

   })

(def exp2
  {:hat (drum-p2
         [:Kit16-Electro]
         [:c1 :2 :c1 :2 :c1 :2 :c1 :2 :c1 :1 :c1 :c2]
         1/4 [:amp 0.7])
   :bass (p/phrase-p
          bass2
          [:E2 :E2 :E2 :E2 :1 :E2 :1 :G2 [:atk 0.001 :decay 2 :cutoff2 1500.92 :amp 0.7] :2]
          1/4 2 [:atk 0.001 :decay 1.2 :cutoff2 1000.92 :amp 0.7])

   :kick (drum-p2
          [:Kit16-Electro]
          [[o-kick [:amp 0.4]] :3])
   :clap (drum-p2
          [:Kit15-Electro]
          [:4 :cl1 :3])
   :clap2 (drum-p2
           [:Kit15-Electro]
           [:4 :cl1 :7 :cl1 :7 :cl1 :7 :cl1 :cl1 :cl1 :1])
   :motif (p/phrase-p
           bass2
           [:B4 :F5 :D5 :E5 :C5 :1 :B4 :1]
           1/4 2 [:cutoff2 3362.20 :atk 0.0012 :decay 3.02 :amp 0.5])
   :prophet (p/phrase-p
             prophet
             [:D3 :14 :C3 :14 :B2 :14 :A2 [:attack 1.81 :decay 4 :cutoff 2125.984 :amp 0.3] :18]
             1/4 0 [:attack 1.81 :decay 2.2 :cutoff 2125.984 :amp 0.3])
   :rise (p/phrase-p
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
          1/4 2 [])
   :bass2 (p/phrase-p
           acid-bass
           [:C2 :C2 :1 :D2 :1 :C2 :D2 :1]
           1/4 0 [:dur 0.47 :amp 0.2])
   :beat1 (p/build-map-p
           [[ b-kick []   ] :1
            [ b-kick []   ] :4
            [ o-snr [] (get-in drum-kits [:Kit16-Electro :CYCdh_ElecK06-Snr01.wav]) []   ] :2
            [ o-snr [] (get-in drum-kits [:Kit16-Electro :CYCdh_ElecK06-Snr01.wav]) []   ] :4
            [ b-kick []   ]
            ])
   :motif2 (p/phrase-p
            bass-synth
            [:E4 :3 :E4 :F4 :E4 :F4 :E4 :F4 :E4 :4 :F4 :G4
             :E4 :D4 :E4 :2]
            1/4 1 [:attack 0.03 :release 1 :detune 0 :bwr 2 :amp 0.7])
   :rise2 (p/phrase-p
           rise-pad
           [[:B4 :F5]
            [:B4 :F5]
            [:A4 :E5]
            [:A4 :E5]
            [:G4 :E5]
            [:G4 :E5]
            [:A4 :E5]
            [:A4 :E5] :6]
           1/4 6 [:t 2])
   })

(def randob
  {:motif1 (p/phrase-p
            bass2
            [:D1 :1 :Bb1 :1 :D1 :1 :Bb1 :| :2 :D1 :1 :Bb1 :| :D1 :1 :Bb1 :1 :Bb1 :1 :G1 :| :Bb1 :7]
            1/8 0 [:decay 1.2283465055030163 :cutoff2 944])
   :overp (p/phrase-p
           overpad
           [:G3 :1 :G4 :1 :Bb4 :1 :C5 :| :2 :F3 :1 :G4 :| :G3 :1 :G4 :1 :Bb4 :1 :C5 :|
            :F3 :1 :F4 :1 :F4         :|]
           1/8 0 [:amp 0.1 :release 0.7 :attack 0.01])
   :drum2 (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            (drum-p2
             [:Kit6-Electro]
             [:k2        :| :k2 :|
              :k2 :1 :s1 :| :k2 :|]
             1/4))
   :motif3 (p/phrase-p
            plucked
            [:E4 :4 :E4 :| :2 :E4 :3 :A3 :| :3 :A3 :| :A3 :2 :D4 :| :D4 :4 :D4 :| :B3 :4 :B3 :| :8]
            1/8 0 [:mistune 0.396850400083647])

   })
(def cheesy
  {:drum1 (p/build-map-p
           [[ (get-in drum-kits [:KurzweilKit02 :CYCdh_Kurz02-Kick01.wav]) []   ] :2
            [ (get-in drum-kits [:KurzweilKit02 :CYCdh_Kurz02-Kick01.wav]) []   ] :|
            [ (get-in drum-kits [:KurzweilKit02 :CYCdh_Kurz02-Snr02.wav]) []   ] :|
            [ (get-in drum-kits [:KurzweilKit02 :CYCdh_Kurz02-Kick01.wav]) []   ] :2
            [ (get-in drum-kits [:KurzweilKit02 :CYCdh_Kurz02-Kick01.wav]) []   ] :|
            [ (get-in drum-kits [:KurzweilKit02 :CYCdh_Kurz02-Snr02.wav]) []   ] [ (get-in drum-kits [:KurzweilKit02 :CYCdh_Kurz02-SdSt.wav]) []   ] :|
            ] 1/4)
   :harmony1 (p/phrase-p
              bass-synth
              [[:G3 :G2] :| :2 [:G3 :G2] :| :| [:G3 :G2] :| [:F3 :F2] :| :2 [:F2 :F3] :| :2 [:F3 :F2] :| :| [:E3 :E2] :| :2 [:E3 :E2] :| :| [:E3 :E2] :| [:Eb2 :Eb3] :| :2 [:Eb2 :Eb3] :| :| [:Eb2 :Eb3] :| [:C2 :C3] :| :2 [:C3 :C2] :| :| [:C2 :C3] :| [:D2 :D3] :| :2 [:D3 :D2] :| :| :|]
              1/4 0 [:attack 0.01 :amp 0.3 :release 1.4 :detune 2.8 :bwr 5.1])
   :motif2 (p/phrase-p
            bpfsaw
            [:G5 :1 :Bb5 :| :C6 :1 :G5 :| :Bb5 :1 :C6 :| :G5 :1 :Bb5 :| :F5 :1 :Bb5 :| :C6 :1 :F5 :| :Bb5 :1 :C6 :| :F5 :1 :Bb5 :| :E5 :1 :Bb5 :| :C6 :1 :E5 :| :Bb5 :1 :C6 :| :E5 :1 :Bb5 :| :Eb5 :1 :Bb5 :| :G5 :1 :Eb5 :| :Bb5 :1 :G5 :| :Eb5 :2 :G5 [:atk 0.01 :dur 3 :rq 0.3] :| :| :| :| :| :| :| :| :|]
            1/4 0 [:atk 0.01 :dur 1.1811023622047243 :rq 0.4566929201910815])
   :motif1 (p/phrase-p
            bass2
            [:G3 :1 :D4 :| :Eb4 :1 :D4 :| :G3 :1 :D4 :| :Eb4 :1 :D4 :| :F3 :1 :D4 :| :Eb4 :1 :D4 :| :F3 :1 :D4 :| :Eb4 :1 :G4 :|]
            1/4 0 [:decay 3.3070867455850435 :cutoff2 2204.7244094488187 :cutoff 3779.527559055118])

   })
