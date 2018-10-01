(ns techno.sketches3)

(def sketch
  {:tabla  (drum-p2
            [:tabla :claves]
            {1 {1 :dhin  2 :ge}
             2 {1 :ge 3 :te 4 :tin}
             3 (fn [d n]
                 (get
                  (p/w-choose
                   {{1 :ta 2 :ta 3 (choose [:ta :tu])} 0.3
                    {1 (p/w-choose {:dhin 0.2 :dha 0.2 :hit6 0.6})
                     2 (choose [:ta :tin :tu :decres])} 0.3
                    {1 :ge 3 (choose [:te :hit1 :hit3]) 4 :tin} 0.4})
                  n) )
             :p-size [3 4]
             }
            1/4)
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
  {:tempo 100
   :klang (p/phrase-p
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

(def wavy
  {:tempo 100
   :drone (p/phrase-p
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
  {:random (drum-p2
            [:Kit5-Electro]
            {1 {1 :k2 5 :k2  9 :s1 11 :c1 12 :c1 13 :c1 15 :s1 16 []}
             2 {1 :k2
                2 (fn [d] (choose [:c1 :c2 nil]))
                3 (fn [d] (choose [:k2 nil]))
                5 (fn [d] (choose [:cl1 :c1]))
                7 :k2
                9 (fn [d] (choose [:k1 :s1 :s2 nil :k2]))
                13 :s2
                }
             :p-size [2 16]
             }
            1/16)
   :sixt (let []
            (drum-p2
             [:Kit3-Acoustic]
             {1 (fn [d n]
                  (cond (= n 1) (keyword (str "k" (choose (range 1 4))))
                        (< n 5) (keyword (str "c" (choose (range 1 4))))
                        (= n 5) (keyword (str "snr" (choose (range 1 4))))
                        (< n 9) (keyword (str (choose ["c" "sd"]) (choose (range 1 4))))
                        (= n 9) (keyword (str (choose ["k" "snr" "t"]) (choose (range 1 4))))
                        (< n 13) (keyword (str (choose ["c" "sd"]) (choose (range 1 4))))
                        (= n 13) (keyword (str "snr" (choose (range 1 4))))
                        true (keyword (str (choose ["cr" "sd" "t"]) (choose (range 1 4))))))
              :p-size [1 16]
              }
             1/16))
   :tempo 60
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

(def cheesy
  {:tempo 120
   :drum1 (p/build-map-p
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



(def cs2
  {:cs (p/phrase-p
              cs80
              [[:G4 :D5] :2 [:D5 :G4] :| :1 :G4 :| :7 [:Eb5 :Ab4] :| :3 [:Ab4 :Eb5] :2 [:Eb5 :Ab4] :| :1 :Ab4 :Eb5 :2 :Eb5 :| :| :2 :Ab4 :2 :Ab4 :| :1 [:D5 :Ab4] :2 [:D5 :Ab4] :| :| :7 [:Ab4 :Ab5] :| :2 :Ab4 :3 :Ab4 :| :1 :Ab4 :1 [:Ab5 :Ab4] :2 :Ab4 :Ab5 :| :| :6 :G5 :| :1 :G5 :2 :G5 :2 :G5 :| :| :4 [:G4 :B5] :2 :G4 :| :2 :G4 :| :3 [:G4 :B5] :| [:G4 :B5] :| [:G4 :B5] :| :| :| :|]
              1/8 0 [:amp 0.4])
   :prophet (let [a [:Ab3 :B3]
                   b [:G3 :B3]
                   c [:G3 :E3]
                   d [:B3 :D3]]
               (p/phrase-p
                prophet
                [a a :5 a a a :5 b b :5 b b b :5 c c :5 c c c :5 d d :5 d d d :|
                 a a :5 a a a :5 b b :5 b b b :5 c c :5 c c c :5 d d :5 d d d :|]
                1/8 3 [:decay 1.5 :attack 0.01 :cutoff 1500 :amp 0.6]))

   :saw (p/phrase-p
              bpfsaw2
              [[:G5 :D5] :1 :G5 :D5 :2 [:G5 :D5] :| :1 [:G5 :D5] :2 :G5 :2 :G5 :| :D5 :1 :G5 :D5 :2 :G5 :| :7 [:D5 :G5] :| :2 [:G5 :D5] :| :6 :Eb5 :| :3 :Eb5 :1 :Ab5 :Eb5 :| [:Ab5 :Eb5] :2 [:Eb5 :Ab5] :2 [:Eb5 :Ab5] :| :2 [:Eb5 :Ab5] :2 [:Ab5 :Eb5] :| :1 [:Ab5 :Eb5] :1 :Eb5 :Ab5 :| :| :| :1 :G5 :Eb5 :2 [:Eb5 :G5] :| [:Eb5 :G5] :2 [:Eb5 :G5] :2 [:Eb5 :G5] :| :1 [:Eb5 :G5] :2 [:G5 :Eb5] :2 [:Eb5 :G5] :| :3 [:G5 :Eb5] :2 [:Eb5 :G5] :| :| :| [:B5 :B4] :2 [:B5 :B4] :2 :B5 :B4 :| :1 :B5 :B4 :2 [:B5 :B4] :| [:B5 :B4] :2 [:B5 :B4] :2 :B5 :B4 :| :| :1 [:B5 :B4 :G4] :3 [:B5 :G4 :B4] :| :B5 [:B4 :G4] :2 [:B5 :G4 :B4] :2 [:B5 :B4 :G4] :|]
              1/8 0 [])
   :beat2 (let []
            (drum-p2
             [:Kit15-Electro]
             {1 {1 :k1 3 :c1 4 :c1}
              2 {1 :k1 2 :c1 3 :c1}
              3 {1 :k1 3 :c1 4 []}
              }
             1/4))

   :beat1 (let []
            (drum-p2
             [:Kit11-Vinyl]
             [:k2  :3 :k1 :1 :k2    :|
              :k2 :3 :k1 :k1 :2 :s5 :|]
             1/8))
   :tempo 87
   })

(def baz
  {:motif1 (p/scale-p
            bass2
            :A3 :phrygian
            {1 {1 [:1 [:decay 2]] 5 :3 6 :2 7 :6}
             2 {1 :4> 3 [:5 :7< {:decay 2}] 8 []}
             }
            1/8 0 [:atk 0.01 :decay 1])

   :harmony2 (p/phrase-p
              bass-synth
              [[:Bb2 :D3] :| :| :| :2 [:D3 :Bb2] :| [:D3 :Bb2] :| :| :4 [:Bb2 :D3] :| :2 [:Bb2 :D3] :| :3 [:E3 :A2] :| :| :| :| :3 :A3 :| :2 :E3 :| :2 :D3 :| :|]
              1/8 0 [:attack 0.2 :release 4.3 :bwr 0.78])
   :harmony1 (p/scale-p
              prophet
              :A4 :phrygian
              {1 {1 :1 3 :2 5 [:6 [:decay 0.3]]}
               2 {1 :4> 3 :7 5 :1> 7 :5}
                                        ;3 {3 [:2 :5 {:decay 1}] 7 [:1 :3 {:decay 2}]}
               4 { 8 []}
               }
              1/8 0 [:attack 0.01 :decay 0.2])

   :drum1 (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            (drum-p2
             [:Kit4-Electro]
             [:k1 :1 :c1 :c1 :1 :c3 :|
              :o1 :1 :s1 :1 :s3 :s3 :|]
             1/8))

   })

(def drone
  {:harmony1 (p/phrase-p
              bpfsaw
              [[:A4 :D4] :| :| [:B4 :E4] :| :3 [:D4 :D5] :| :2 :B4 :| :2 [:G3 :E4 {:dur 3}] :| :| :|]
              1/4 0 [:dur 2 :atk 0.6 :detune 0 :rq 0.15 :amp 0.4 :pan 0.0])
   :harmony2 (p/phrase-p
              bass-synth
              [[:F3 :B3] :| :| [:E3 :A3] :| :2 [:C4 :D3 {:release 5}]
               :| :| :| :| :|]
              1/4 0 [:attack 0.1 :amp 1.0 :release 3 :detune 3.0 :bwr 0.7])
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
             [:Kit14-Acoustic]
             {1 {1 :sd1 5 :sd1 7 :rim1}
              2 (fn [d n]
                  ;; (if (odd? n)
                  ;;   (p/w-choose {[:s1 [:amp 0.5]] 0.3 [:s2 [:amp 0.5]] 0.2 nil 0.3 :sd3 0.2}))
                  )
              :p-size [2 8]
              }
             1/8))
   :drum1 (let [o o-kick
                s o-snr
                b b-kick
                bs b-snr
                c o-clap
                h o-hat
                d dirty-kick
                g g-kick
                r r-kick]
            {:div 8
             :fn (fn [p key b n]
                   (cond (= n 1) [(drum-s [:Kit14-Acoustic] :k1) []]
                         )
                   )
             :p-size [7 8]
             })
   })

(def plk
  {:tempo 100
   :motif1 (p/scale-p
            plk-bass
            :D3 :major
            [:1 :05 :1 :|
             :|
             :7<        :|
             :|
             :3 :05 :3 :|
             :|
             :2        :| :|
             ]
            1/8 0 [:dur 0.5 :amp 1.0 :plk 2.0 ])

   :drum2 (let []
            (drum-p2
             [:Kit3-Acoustic]
             [:c4 :1 :c3 :c4                       :|
              :2 (fn [d] (choose [:cr1 :cr2 nil]))  :|
              :c4 :1 :c3 :c4                       :|
              :2 (fn [d] (choose [:sd1 :s1 nil]))
              (fn [d] (choose [:sd3 :s1 :sd4 nil])) :|]
             1/4))

   :drum1 (drum-p2
           [:Kit13-Acoustic]
           [:| :s2 :|]
           1/8)

   :harmony1 (p/scale-p
              bpfsaw
              :D3 :major
              [:7 :03 [:1> [:amp 0.05]] :|
               :|
               :|
               :03 :6>                  :|
               :|
               :|
               :04 :5>                  :|
               :|
               :|
               :06 [:2> :3]             :| :| :|]
              1/8 0 [:amp 0.2 :atk 0.3 :dur 3 :rq 0.9])


   })

(def gen-kicks
  {:drum1 (let []
            (drum-p2
             [:Kit1-Acousticclose]
             [(fn [d n] (cond (< n 7)
                             (p/w-choose {(keyword (str "k" (range 1 5))) (if (odd? n) 0.8 0.3)
                                          nil (if (odd? n) 0.2 0.7)})
                             (= n 7) (choose [:k7 :k8]))
                ) :|]
             1/8))

   :drum2 (let []
            (drum-p2
             [:Kit2-Acousticroom]
             [(fn [d n] (cond (< n 7)
                             (p/w-choose {(keyword (str
                                                    "c"
                                                    (range 1 5))) (if (odd? n) 0.8 0.3)
                                          nil (if (odd? n) 0.2 0.7)})
                             (= n 7) (choose [:c7 :c8 :c9])
                             (= n 8) (p/w-choose {(choose [:c7 :c8 :c9]) 0.2 nil 0.8}))
                ) :|]
             1/8))
   :drum3 (let []
            (drum-p2
             [:Kit17-Electro]
             [:k1 :1]
             1/4))
   :bass (p/scale-p
          bass-synth
          :C4 :major
          [:2<<                    :|
           :|
           :6<< :05 :1<            :|
           :04 :2<                 :|
           :04 [:3<< [:release 3]] :|
           :|
           :| :|]
          1/8 0 [:attack 1 :amp 1.0 :release 2 :detune 3.0 :bwr 0.4 ])

   })
(def sketch3
  {:tempo 100
   :motif1 (p/scale-p
            plucked
            :C4 :major
            [:2> :03 :2>                               :|
             :1> :03 [:7 :3]                           :|
             :04 :6                                    :|
             :04 [:7 :2]                               :|
             :04 :1>                                   :|
             :04 [:5 :1 {:release 3}]                  :|
             :|
             :|
             :2> :03 :2>                               :|
             :1> :03 [:7 :3]                           :|
             :04 :6                                    :|
             :04 [:7 :2]                               :|
             :04 :1>                                   :|
             :04 [:5> :3 :1> [:amp 0.05] {:release 3}] :|
             :|
             :|
             ]
            1/8 0 [:amp 0.1 :mistune 1.008 :pos 0.8 :gc 0.01 :mp 1 :c3 20 :release 2.0 ])

   :harmony2 (p/scale-p
              sawPad
              :C4 :major
              [[:5< :6]                                               :|
               :07 :5                                                 :|
               :|
               :05 [:5< [:gate 0]] [:6 [:gate 0] :5 [:gate 0] :3] :6< :|
               :|
               :06 [:3 [:gate 0] :5]                                  :|
               :|
               :04 [:5 [:gate 0] :7] [:6< [:gate 0] :5<]              :|
               :|
               :|
               :|
               :07 [:7 [:gate 0]]                                     :|
               [:5< [:gate 0]]                                        :| :| :| :|]
              1/8 0 [:gate 1.0 :amp 0.3 :cutoff 2000 :cutoff2 1000 :fdur 3.0 :release 1.0 ])

   :harmony1 (p/scale-p
              fmTest
              :C4 :major
              [:3<                      :|
               :|
               :|
               :01  [:7<< :3< [:gate 0]] :|
               :01 [:7<< [:gate 0] :1<] :|
               :|
               :|
               :06 [:1< [:gate 0]] :3<  :|
               :|
               :|
               :|
               :7<< [:3< [:gate 0]]     :|
               [:7<< [:gate 0] :6<]     :|
               :|
               :|
               :01 [:6< [:gate 0]]      :|
               ]
              1/8 0 [:gate 1.0 :amp 3 ])

   :drum2 (drum-p2
           [:Kit15-Electro]
           [                                                                                :|
            :02 [(drum-s [:Kit17-Electro] :snr2) []]                                        :|
            :02 [(drum-s [:Kit17-Electro] :snr2) []]                                        :|
            :1 [(drum-s [:Kit17-Electro] :snr2) []] :1 [(drum-s [:Kit17-Electro] :snr2) []] :|
            ]
           1/4)
   :drum1 (drum-p2
           [:Kit15-Electro]
           [[(drum-s [:Kit13-Acoustic] :kick4) []] :1 [(drum-s [:Kit13-Acoustic] :c2) []] [(drum-s [:Kit13-Acoustic] :ophat1) []] [(drum-s [:Kit13-Acoustic] :c2) []] :1 [(drum-s [:Kit13-Acoustic] :c2) []]     :|
            :02 [(drum-s [:Kit13-Acoustic] :c2) [] (drum-s [:Kit13-Acoustic] :ophat1) []] :1 [(drum-s [:Kit13-Acoustic] :ophat1) [] (drum-s [:Kit13-Acoustic] :c2) []]                                           :|
            [(drum-s [:Kit13-Acoustic] :c2) [] (drum-s [:Kit13-Acoustic] :kick4) []] :03 [(drum-s [:Kit13-Acoustic] :c2) []]                                                                                     :|
            [(drum-s [:Kit13-Acoustic] :kick4) []] :1 [(drum-s [:Kit13-Acoustic] :kick4) []] :1 [(drum-s [:Kit13-Acoustic] :c2) [] (drum-s [:Kit13-Acoustic] :ophat1) []] :1 [(drum-s [:Kit13-Acoustic] :c2) []] :|
            ]
           1/8)


   })
