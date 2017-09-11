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
