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
            1/8 0 [])
   :motif2 (p/phrase-p
            overpad
            []
            1/8 0 [])
   :motif3 (p/phrase-p
            bass2
            []
            1/8 0 [])
   :motif4 (p/phrase-p
            plk-bass
            []
            1/8 0 [])
   :motif5 (p/phrase-p
            bpfsaw
            []
            1/8 0 [])
   :harmony1 (p/phrase-p
            bpfsaw
            []
            1/8 0 [])
   :harmony2 (p/phrase-p
            overpad
            []
            1/8 0 [])
   :harmony3 (p/phrase-p
            bass2
            []
            1/8 0 [])
   :harmony4 (p/phrase-p
            plk-bass
            []
            1/8 0 [])
   :harmony5 (p/phrase-p
            bpfsaw
            []
            1/8 0 [])
   :metronome (p/phrase-p
               bing
               [:C4 :|]
               1/4)
   }
  )

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
   :tempo 100
   })
(def chill
  {:tempo 97
   :mainsaw    (p/phrase-p
                bpfsaw
                [(chord :A3 :m7) :8 (chord :B3 :m7) :6]
                1/4 0 [:rq 0.6 :dur 1.6 :amp 0.08 :atk 0.7])
   :lead    (p/phrase-p
             bass2
             [:D4 :1 :G4 :2 :C4 :2 :A4 :3]
             1/4 0 [:dur 0.3 :decay 3 :amp 1.5])
   :kicks    (p/build-map-p
              [[o-kick []] :1 [o-hat []] :2 [o-hat []] :2]
              1/4)
   :hat  (p/build-map-p
          [[(drum-s [:KurzweilKit03] :c1) []] :|]
          1/8)
   :claps    (p/build-map-p

              [:5 [o-clap []
                   (drum-s [:Kit15-Electro] :cl1) [:amp 2]
                   ]]
              1/4)
   :beat2 (let []
            (drum-p2
             [:Kit6-Electro]
             {1 {1 [[o-kick []]] 3 :c1 5 :c1 7 :c2 8 []}
              2 (fn [d n]
                  (get
                   (p/w-choose
                    {{1 [[o-kick []]] 3 :c1 5 :c1 7 :c2 8 []} 0.1
                     {1 (p/w-choose {[[o-snr []]] 0.7 [[o-kick []]] 0.3})
                      3 (p/w-choose {:c1 0.3 :cl1 0.3 :o1 0.2 (get-in d [2 1]) 0.2})
                      5 (get-in d (p/w-choose {[2 1] 0.7 [2 3] 0.3}))
                      7 (get-in d (p/w-choose {[2 5] 0.7 [2 1] 0.3}))
                      8 []} 0.9})
                   n))
              :p-size [1 8]
              }
             1/8))
   :bass2 (p/scale-p
           acid-bass
           :C2 :major
           {1 {1 :2 3 :2 7 :4 8 []}
            2 {1 :6<
               ;; 3 (fn [d] (p/w-choose {[:2 :5] 0.2 nil 0.8}))
               ;; 5 (fn [d] (p/w-choose {[:7< :3< {:dur 0.5}] 0.3 nil 0.7}))
               8 []}
            }
           1/8 0 [:dur 0.23622047244094488])

   :bass   (p/phrase-p
            acid-bass
            [:F1 :1 :B1 :2 :G1 :2 :A1 :3]
            1/4 0 [:dur 0.3])
   :lead2 (p/scale-p
           bass2
           :C4 :major
           [:6< :01 :6< :03 :6<   :|
            :02 :4 :01 :1> :01 :2> :|
            :04 :6<                :|
            :3< :03 :6<            :|]
           1/8 0 [:atk 0.001 :f-dur 0.1 :decay 2 :amp 2.0 :cutoff 2000.0 :cutoff2 4000])
   :zap (assoc
         (p/build-map-p
          [[ zap [:freq2 400 :dur 0.3 :amp 1 :freq1 698.4564628660078 :amp 0.3] ] :5
           [ zap [:freq2 400 :dur 0.3 :amp 1 :freq1 987.7666025122483 :amp 0.3]  ]   :|
           :4
           [ zap [:freq2 400 :dur 0.3 :amp 1 :freq1 1046.5022612023945 :amp 0.3]  ]  :|
           :4
           [ zap [:freq2 400 :dur 0.3 :amp 1 :freq1 1174.6590716696303 :amp 0.3]   ] :1
           [ zap [:freq2 400 :dur 0.3 :amp 1 :freq1 783.9908719634985 :amp 0.3]    ] :|
           [ zap [:freq2 400 :dur 0.3 :amp 1 :freq1 1046.5022612023945 :amp 0.3]  ]  :|
           ] 1/8)
         :fx {:reverb [p/p-reverb
                       :roomsize 50
                       :revtime 1
                       :damping 0.8
                       :inputbw 0.4 :drylevel 2 :earlylevel 1 :taillevel 4]})
   :brsh (assoc
          (drum-p2
           [:KurzweilKit05]
           [(fn [d n]
              (if (odd? n)
                (p/w-choose
                 {[:brsh2 :brsh1 [:amp 0.3]] 0.9
                  [:brsh3 [:amp 0.3]] 0.1
                  }))
              ) :|]
           1/4)
          :p-size [1 3])
   :beat3 (drum-p2
           [:Kit1-Acousticclose]
           [(fn [d n]
              (p/w-choose
               {(keyword (str (choose ["rim" "s"]) (choose (range 3 8)))) (if (odd? n) 0.9 0.5)
                nil (if (odd? n) 0.1 0.5)})
              ) :|]
           1/4)
   :cs (assoc
        (p/scale-p
         cs80
         :C3 :minor
         [[:4b>> [:atk 0.4]]      :|
          :06 :2>>                :|
          :|
          :1>>> [:amp 0.3 :dur 3] :|
          :|
          :015 :1>                :|
          :010 :2>                :|
          :07 :4b>                :|
          :|
          :01 :1b                 :|
          :|
          :02 :4b                 :|
          :| :| :| :|]
         1/16 0 [:amp 0.5 :dur 2 :atk 0.01 :rq 0.5 :cutoff 4000 :dtune 0.002 :vibrate 4.0 :vibdepth 0.015 :freq-lag 0.1 ])
        :s 1
                                        ;:fx {:delay [p/p-delay :max-delay 2 :delay 0.1 :decay 4]}
        )
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

(def house3
  {:tempo 120
   :kick (p/build-map-p
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
  {:tempo 103
   :hat (drum-p2
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

(def manic
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
             [:Kit14-Acoustic]
             {:div 1/8
              1 {1 [[dirty-kick [:amp 0.6]]] 4 []}
              }))

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
             [:Kit14-Acoustic]
             {
              1 {1 :sd1 4 :sd2 5 :sd2 7 :sd3}
              2 {3 :t1 4 :t1 5 :t2 8 []}
              }
             1/8))

   :harmony1 (p/phrase-p
              bpfsaw
              [[:F#4 :Eb4] :| :5 [:Ab4 :Eb4] :| :| :3 [:Bb4 :Eb4] :| :| [:F#4 :D4] :| :| :|]
              1/8 0 [:dur 2 :amp 0.5 :atk 0.3 :rq 0.5])
   :harmony2 (p/phrase-p
              bpfsaw2
              [[:F#5 :D5] :1 :F#5 :D5 :1 :F#5 :D5 :| [:D5 :F#5] :| :1 [:Ab5 :D5] :1 [:Ab5 :D5] :1 [:Ab5 :D5] :1 :D5 :| :| :| :7 [:F#5 :C5] :| :1 [:F#5 :C5] :1 [:F#5 :C5] :| :|]
              1/8 0 [:lsf 1000])
   :motif1 (p/phrase-p
            bass2
            [:D4 :3 :F#4 :| :Ab4 :1 :Bb4 :1 :Ab4 :Bb4 :| :1 :F#4 :2 :F#4 :1 :F4 :| :|]
            1/8 0 [:decay 4])

   })

(def bassy
  {:tempo 83
   :beat2 (drum-p2
           [:Kit6-Electro]
           [:k1 :1 :k2                           :|
            [o-kick []]
            :5 [(drum-s [:Kit7-Electro] :k1) []] :|]
           1/8)

   :cl (drum-p2
        [:Kit8-Vinyl]
        [:c1 :1]
        1/4)

   :saw (p/phrase-p
              bpfsaw2
              [[:E4 :E3] :| :| :| [:A3 :A4] :| :| :| [:D4 :D3] :| :|]
              1/8 0 [:t 3 :detune 0])
   :kicks  (let [c1 [(drum-s [:Kit7-Electro] :c1) []]
                 c2 [(drum-s [:Kit7-Electro] :c2) []]]
             (drum-p2
              [:Kit6-Electro]
              [:k2       :|
               :k2 :3 c1 :|]
              1/8))
   :sd  (let [o o-kick
                 s o-snr
                 b b-kick
                 bs b-snr
                 c o-clap
                 h o-hat
                 d dirty-kick
                 g g-kick
                 r r-kick]
             (drum-p2
              [:Kit3-Acoustic]
              [ :| :1 :sd6 :1 :sd7 :|]
              1/4))
   :crash  (let [o o-kick
                 s o-snr
                 b b-kick
                 bs b-snr
                 c o-clap
                 h o-hat
                 d dirty-kick
                 g g-kick
                 r r-kick]
             (drum-p2
              [:Kit3-Acoustic]
              [:cr4 :1 :cr1 :|
               :cr6 :cr1    :|]
              1/4))
   :bass  (p/phrase-p
               bass-synth
               [[:D4 :G3] :| :4 [:F3 :C4] :| :| :1 :E4 :A3 :| :7 [:E3 :B3] :| :| :3 [:G3 :D4] :| :|]
               1/8 0 [:release 2 :detune 0 :attack 1 :bwr 0.1 :amp 0.1])
   :drone (p/scale-p
           drone-noise
           :C5 :major
           {1 (fn [d n]
                (if (odd? n)
                  [(keyword
                    (str (choose (range 1 8))
                         (p/w-choose {">" 0.1 "" 0.6 "<" 0.3})))
                   [:dur (max 4 (rand-int 6))]]
                  ))
            4 {8 []}
            }
           1/8 0 [:amp 0.3 :dur 4 ])
   :zap (assoc
         {:div 4
          :p-size [4 4]
          1 {1 (fn [p key]
                 [zap2 [:freq1 (midi->hz (choose (scale :C4 :major)))
                        :dur 0.4
                                        ;:dur (max 0.3 (rand 1.5))
                        :freq2 (midi->hz (choose (scale :C5 :major)))]]
                 )}
          }
         :fx {:delay [p/p-delay :max-delay 2 :delay 0.2 :decay 3]
              :delay2 [p/p-delay :max-delay 2 :delay 0.4 :decay 4]
              :reverb [p/p-reverb
                       :roomsize 3
                       :revtime 1
                       :damping 0.8
                       :inputbw 0.01 :drylevel 3 :earlylevel 1 :taillevel 1]
              })
   :bpfsaw (assoc
            (p/scale-p
             bpfsaw
             :C4 :major
             [:6> :06 :2>                   :|
              :|
              :|
              :|
              :01 :6                        :|
              :02 (fn [d] (choose [:3> :4])) :|
              :|
              :|
              :02 :2>                       :|
              :02 (fn [d] (choose [:1> :6])) :|
              :|
              :|
              :| :| :| :|]
             1/8 0 [:dur 1.0 :atk 0.01 :detune 0.0 :rq 0.5 :amp 0.2 :pan 0.0 ])
            :fx {:delay [p/p-delay :max-delay 2 :delay 0.2 :decay 3]
                 :delay2 [p/p-delay :max-delay 2 :delay 0.4 :decay 6]
                 :reverb [p/p-reverb
                          :roomsize 3
                          :revtime 1
                          :damping 0.8
                          :inputbw 0.01 :drylevel 3 :earlylevel 1 :taillevel 1]
                 })

   })


(def cs
  {:motif2 (p/phrase-p
            prophet
            [:F4 :5 :C4 :| :4 :F#3 :| :2 :F3 :| :C4  :| :| :| :| :| :| :|
             :F4 :5 :C4 :| :4 :F#3 :| :2 :F3 :| :C4  :| :| :| :| :| :| :|]
            1/8 0 [:attack 0.01 :decay 4 :cutoff 1535])
   :motif1 (p/phrase-p
            plk-bass
            [:Ab2 :1 :F#2 :3 :Ab2 :| :F#2 :3 :Ab2 :1 :F#2 :| :| :F#2 :5 :F2 :| :8]
            1/8 0 [])
   :harmony1 (p/phrase-p
              cs80
              [[:F5 :Bb5] :| :| :6 [:F5 :Ab5] :| :| :| :5 [:Eb5 :C5] :| :| :| :4 [:C5 :F5] :| :| :| :|
               [:F5 :Bb5] :| :| :6 [:F5 :Ab5 {:dur 2}]
               :|  :| :5 [:Eb5 :C5 {:dur 2}] :|  :| [:C5 :F5 {:dur 2}] :|  :|]
              1/8 0 [:amp 0.4 :dur 3])
   :drum3 (let []
            (drum-p2
             [:Kit6-Electro]
             {1 {1 :o 8 []}
              }
             1/8))
   :drum1 (let []
            (drum-p2
             [:Kit6-Electro]
             {1 {1 :s1 5 :o}
              2 {1 :k1 3 :t2 5 :o 7 :t3 8 []}
              }
             1/8))

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
(def house2
  {:harmony (p/phrase-p
             rise-fall-pad2
             [[:C3 :G3 :E3 :B3] [:F3 :C4 :A3 :E4] :34]
             1/4 32 [:t 5])
   :motif (p/phrase-p
           bass-synth
           [[:E3 :B3] [:A3 :F3] :34]
           1/4 32 [:release 6 :amp 0.4 :detune 4])
   :bass {:div 4
          :p-size [17 4]
          :fn (fn [p k b no]
                (cond
                  ;; (or (and (= no 4) (odd? b))
                  ;;     (= no 1))
                  ;; nil
                  true (let [n (note (cond (< (p/get-beat b no 4) 34) :E3
                                           (< (p/get-beat b no 4) 52) :D3
                                           true :F3))
                             action (vector plk-bass [:note (note n) :amp 0.6]
                                            wire-bass [:amp 0.4 :dur 2 :coef 0.01 :decay 0.8])]
                         action)))}
   :clap (drum-p2
          [:Kit15-Electro]
          [:4 [:cl1 [o-clap []]] :3]
          1/4)
   :motif2 (p/phrase-p
           bass-synth
           [[:E4 :B3] [:A4 [:amp 0.3] :F4 [:amp 0.3]] :34]
           1/4 32 [:release 6 :amp 0.4 :detune 4])
   :motif3 (p/phrase-p
            reverb-test
            [:A4 :B4 :D4 :C4 :B4]
            1/4 2)
   :kick (drum-p2
          [:Kit4-Electro :Kit3-Acoustic]
          [:k1 :1 :o1 [:amp 0.3] :1]
          1/4)
   :cl  (drum-p2
           [:Kit16-Electro]
           [:c1 :c1 :c2 :2 :c1 :2]
           1/4)
   :snr (drum-p2
         [:Kit3-Acoustic :Kit16-Electro :Kit4-Electro]
         [:2 :s3 :1]
         1/4)
   :t (drum-p2
       [:Kit5-Electro]
       [:cl1 :2]
       1/4)
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

(def sad
  {:harmony1 (p/scale-p
              bowed
              :D4 :major
              [:2 :06 :2 :03 :2        :|
               :03 :1 :04 :1           :|
               :|
               :|
               :02 :1 :03 :1 :03 :1    :|
               :02 :2 :03 :2           :|
               :07 :7< :04 :7<         :|
               :03 :7<                 :|
               :1 :03 :1 :03 :1        :|
               :05 :2 :05 :2           :|
               :|
               :|
               :02 :2> :04 :2> :03 :2> :|
               :010 :1>                :|
               :1> :05 :1>             :|
               :|
               :013 :7                 :|
               :02 :7 :03 :7 :04 :7    :|
               :04 :2> :05 :2>         :|
               :2> :014 :6             :|
               :04 :6 :04 :6           :|
               :08 :5 :03 :5           :|
               :| :|]
              1/16 0 [:atk 0.2 :amp-b 0.5 :start 0.1 :end 0.7 :force 1.0 :dur 2.0 :c1 0.25 :c3 31.0 :amp 0.1 ])

   :motif1 (p/scale-p
            bpfsaw
            :D4 :major
            [:1 :06 :3b :06 :5     :|
             :04 :1 :06 :3b        :|
             :02 :5 :06 :1 :05 :3b :|
             :06 :5 :05 :1         :|
             :03 :3b :06 :5        :|
             :7< :06 :3b :06 :5    :|
             :05 :7< :06 :3b       :|
             :03 :6 :05 :7<        :|
             :3b :07 :5 :06 :7<    :|
             :06 :3b :07 :5        :| :| :|]
            1/16 0 [:dur 1 :atk 0.01 :detune 0.0 :rq 0.6 :amp 0.3 :pan 0.0 ])
   :bass (p/scale-p
          bass-synth
          :D3 :major
          [:1 :01 :1 :02 :1 :01 :1    :|
           :05 :2                     :|
           :2 :01 :2                  :|
           :03 :3b :02 :3b            :|
           :3b                        :|
           :05 :7<                    :|
           :7< :01 [:7< [:release 2]] :| :| :| :| :| :|]
          1/8 0 [:attack 2 :amp 0.3 :release 1.0 :detune 1.0 :bwr 0.5 ])
   })

(def techno-exp
  {:drum2 (drum-p2
           [:Kit15-Electro]
           [[dirty-kick [:amp 0.6]] :1]
           1/4)

   :drum1 (let []
            (drum-p2
             [:Kit14-Acoustic]
             [:sd3
              (fn [d] (p/w-choose {:c2 0.2 :c3 0.1 nil 0.7}))
              (fn [d] (p/w-choose {:c2 0.2 :c3 0.1 nil 0.7}))
              :1 :sd1 (fn [d] (choose [:sd2 :sd1 nil]))
              (fn [d] (p/w-choose {:sd3 0.3
                                  :sd1 0.2
                                  :sd2 0.2
                                  nil 0.3})) :|]
             1/8))

   :harmony1 (p/scale-p
              bass2
              :D2 :major
              [:7<     :|
               :|
               :01 :2  :|
               :|
               :05 :4< :| :| :| :|]
              1/16 0 [:atk 0.5 :f-dur 0.001 :echo 0 :decay 6 :amp 1.0 :cutoff 2000.0 :cutoff2 2000.0 ])

   :harmony2 (p/scale-p
              harmonic
              :D2 :major
              [:7>       :|
               :011 :5>> :|
               :|
               :013 :1>> :|
               :|
               :013 :5>  :| :| :|]
              1/16 0 [:amp 0.5 :dur 2.4 ])


   })
