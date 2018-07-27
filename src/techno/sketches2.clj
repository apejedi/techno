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
  (def player (get-s 80 {:div 8}))
  (def player 21)
  (stop-s player)
  (p/set-sp player 90)
  (p/rm-p player :kick)
  (let [patterns {:kick (drum-p2
                         [:Kit4-Electro :Kit10-Vinyl]
                         [:k1 :|]
                         1/8)
                  :hats (drum-p2
                         [:Kit10-Vinyl]
                         [:o1 :| :c1 :c2 :1 :c1 :|
                          :c2 :c1 :c1 :|]
                         1/4)
                  :clap (drum-p2
                         [:Kit15-Electro]
                         [:cl1 :| :|]
                         1/4)
                  :bass (p/phrase-p
                         acid-bass
                         [:C2 :3 :A1 :1 :B1 :| :| :|]
                         1/8 0 [:dur 0.4])
                  }]
    ;(p/rm-p player :all)
    ;(p/add-p player (:hats patterns) :hats)
    (p/add-p player (:kick patterns) :kick)
    ;; (doseq [[k p] patterns]
    ;;   (p/add-p player p k)
    ;;   )
    )
  )

(def template
  {:drum1 (drum-p2
           [:Kit15-Electro]
           []
           1/4)
   :drum2 (drum-p2
           [:Kit15-Electro]
           []
           1/4)
   :drum3 (drum-p2
           [:Kit15-Electro]
           []
           1/4)
   :drum4 (drum-p2
           [:Kit15-Electro]
           []
           1/4)
   :drum5 (drum-p2
           [:Kit15-Electro]
           []
           1/4)
   :drum6 (drum-p2
           [:Kit15-Electro]
           []
           1/4)
   :drum7 (drum-p2
           [:Kit15-Electro]
           []
           1/4)
   :drum8 (drum-p2
           [:Kit15-Electro]
           []
           1/4)
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


(def exp3
  {:tempo 100
   :beat1 (p/build-map-p
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
   :harmony (p/scale-p
             prophet
             :A3 :major
             [[:3 :6<] :02 [:3 :6<] :|
              :02 [:3 :6<]          :|
              :01 [:3 :6<]          :|
              [:3 :6<]              :|
              [:1 :4<] :02 [:1 :4<] :|
              :02 [:1 :4<]          :|
              :01 [:1 :4<]          :|
              [:1 :4<]              :|
              [:2 :5<] :02 [:2 :5<] :|
              :02 [:2 :5<]          :|
              :01 [:2 :5<]          :|
              [:2 :5<]              :|
              ]
             1/4 0 [:attack 0.001 :decay 1.3 :amp 0.5 :cutoff 2000])
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
   :motif2 (p/scale-p
            bass-synth
            :C4 :major
            [[:2b> :5b] :05 [:2b> :5b] :|
             :04 [:2b> :5b]            :|
             :02 [:2b> :5b]            :|
             [:2b> :5b] :03 [:5b :2b>] :|
             [:5# :2b>] :05 [:5# :2b>] :|
             :04 [:2b> :5#]            :|
             :02 [:2b> :5#]            :|
             [:2b> :5#] :03 [:2b> :5#] :|
             [:2b> :6] :05 [:2b> :6]   :|
             :04 [:2b> :6]             :|
             :02 [:6 :2b>]             :|
             [:7 :2b>] :03 [:7 :2b>]   :|]
            1/8 0 [:attack 0.01 :amp 1.0 :release 1 :detune 3.0 :bwr 0.6 ])
   })
(def chill
  {:tempo 97
   :kicks     (p/build-map-p
               [[o-kick []] :1 [o-hat []] :2 [o-hat []] :2]
               1/4)
   :lead     (p/phrase-p
              bass2
              [:D4 :1 :G4 :2 :C4 :2 :A4 :3]
              1/4 0 [:dur 0.3 :decay 3 :amp 1.5])
   :lead2  (p/scale-p
            bass2
            :C4 :major
            [:6< :01 :6< :03 :6<   :|
             :02 :4 :01 :1> :01 :2> :|
             :04 :6<                :|
             :3< :03 :6<            :|]
            1/8 0 [:atk 0.001 :f-dur 0.1 :decay 2 :amp 2.0 :cutoff 2000.0 :cutoff2 4000])
   :lead3  (let [a [:7 :05 :6                                                                    :|
                    :04 :7                                                                       :|
                    :02 :6                                                                       :|
                    :3 :03 :6 :01 (fn [d] (choose [[(choose [:7> :6> :3>]) [:release 0.3]] nil])) :|]
                 b [:7 :03 :6 :|
                    :04 :3>   :|
                    :04 :7    :|
                    :04 :6    :|
                    :7 :03 :6 :|
                    :04 :3    :|
                    :04 :6    :| :|]]
             (p/scale-p
              bass2
              :C4 :major
              b
              1/8 0 [:amp 2.0 :decay 1 :echo 0]))
   :mainsaw (merge
             (p/phrase-p
              bpfsaw
              [(chord :A3 :m7) :8 (chord :B3 :m7) :6
                                        ;(chord :G3 :M7) :| :| (chord :C4 :M7) :| :|
               ]
              1/4 0 [:rq 0.6 :dur 1.6 :amp 0.08 :atk 0.7])
             {:fx {:low [p/p-peak-eq :freq 400 :db -18]}})

   :sc303  (merge
            (p/scale-p
             sc303
             :C2 :major
             [(fn [d n]
                (if (odd? n)
                  [(choose [:2 :5 :1])
                   [:sus (max 0.2 (rand 0.7))
                    :cutoff (max 300 (rand 2000))
                    :dec (max 0.5 (rand))
                    :wave (choose [0 1])
                    ]])
                ) :|]
             1/8 0 [:amp 0.2 :res 0.2
                                        ;:env 2000
                    ])
            {:mono true})
   :zap  (assoc
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
   :hat   (p/build-map-p
           [[(drum-s [:KurzweilKit03] :c1) []] :|]
           1/8)
   :cs  (assoc
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
   :claps     (p/build-map-p

               [:5 [(drum-s [:Kit15-Electro] :cl1) [:amp 2]
                    ]]
               1/4)
   :brsh  (assoc
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
   :beat3  (drum-p2
            [:Kit1-Acousticclose]
            [(fn [d n]
               (p/w-choose
                {(keyword (str (choose ["rim" "s"]) (choose (range 3 8)))) (if (odd? n) 0.9 0.5)
                 nil (if (odd? n) 0.1 0.5)})
               ) :|]
            1/4)
   :beat2  (let []
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
   :bass2 (merge
           (p/scale-p
            acid-bass
            :C2 :major
            {1 {1 :2 3 :2 7 :4 8 []}
             2 {1 :6<
                3 (fn [d] (p/w-choose {[:2 :5] 0.2 nil 0.8}))
                5 (fn [d] (p/w-choose {[:7< :3< {:dur 0.5}] 0.3 nil 0.7}))
                8 []}
             }
            1/8 0 [:dur 0.23622047244094488])
           {:fx {:comp [p/p-compander :thresh 0.3 :above 0.5]}}
           )
   :bass (merge
          (p/phrase-p
           acid-bass
           [:F1 :1 :B1 :2 :G1 :2 :A1 :3]
           1/4 0 [:dur 0.3 :amp 1])
          {:fx {:hi [p/p-hi-shelf :freq 500 :db -18]
                :comp [p/p-compander :thresh 0.3 :above 0.001]
                }}
          )

   })






(def house3
  {:tempo 120
   :kick (p/build-map-p
          [[(drum-s [:Kit4-Electro] :k1) [:amp 0.4]] :3]
          )
   :lead (p/scale-p
          bass2
          :C4 :major
          [:2> :05 :6> :|
           :04 :2>     :|
           :02 :6>     :|
           :2> :03 :4  :|
           ]
          1/8 0 [:decay 1.2 :echo 0 :atk 0.01 :cutoff 4000 :f-dur 0.3])
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
   :bass2 (p/phrase-p
           bass2
           [:A4 :B4 :C4 :D4 :D4 :1 :D4 :1]
           1/4 2 [:decay 2])
   :bass3 (p/phrase-p
           bass2
           [:E5 :D5 :C5 :B4 :6]
           1/4 2 [:decay 2 :cutoff2 4000])
   :ohat (drum-p2
          [:Kit6-Electro]
          [:2 :o1 :|]
          1/4)

   :bongos (drum-p2
            [:Bongos :claves]
            [(fn [d n]
               (cond
                 (odd? n)
                 (keyword (str (choose ["b" "hit"]) (choose (range 1 8)))))
               ) :|]
            1/8)
   :transition (p/scale-p
                klang-test
                :C4 :major
                [[:4 :6] :05 [:4 :6]     :|
                 :05 [:5 :4>]            :|
                 :04 [:5 :4>]            :|
                 :02 [:4> :5]            :|]
                1/8 0 [:amp 0.1 :atk 0.01 :dur 1.4 :attack 0.01 :decay 1])
   :lead2 (p/scale-p
           reverb-test
           :C4 :major
           [:2> :01 :3> :01 :6> :01
            ]
           1/8 0 [:attack 0.02 :decay 0.3 :amp 0.2 :delay-time 0.2 ])
   :drum2 (let []
            (drum-p2
             [:Kit7-Electro]
             {1 {1 [[o-kick []]] 3 :c1 5 :c1 7 :c2 8 []}
              2 (fn [d n]
                  (get
                   (p/w-choose
                    {{1 [[o-kick []]] 3 :c1 5 :c1 7 :c2 8 []} 0.1
                     {1 (p/w-choose {:s1 0.7 :k1 0.3})
                      3 (p/w-choose {:c1 0.3 :cl1 0.3 :o1 0.2 (get-in d [2 1]) 0.2})
                      5 (get-in d (p/w-choose {[2 1] 0.7 [2 3] 0.3}))
                      7 (get-in d (p/w-choose {[2 5] 0.7 [2 1] 0.3}))
                      8 []} 0.9})
                   n))
              :p-size [2 8]
              }
             1/8))
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











(def house2
  {:harmony (p/phrase-p
             rise-fall-pad2
             [[:C3 :G3 :E3 :B3] [:F3 :C4 :A3 :E4] :34]
             1/4 32 [:t 5 :lsf 400 :ldb -18])

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
                             action (vector plk-bass [:note (note n) :amp 0.4 :plk 1]
                                            ;wire-bass [:amp 0.4 :dur 2 :coef 0.01 :decay 0.8]
                                            )]
                         action)))}
   :clap (drum-p2
          [:Kit15-Electro]
          [:4 :cl1 :3]
          1/4)
   :motif2 (p/phrase-p
           bass-synth
           [[:E4 :B3] [:A4 [:amp 0.3] :F4 [:amp 0.3]] :34]
           1/4 32 [:release 6 :amp 0.4 :detune 4])
   :motif3 (p/phrase-p
            bpfsaw
            [:A4 :B4 :D4 :C4 :B4 :2]
            1/4 2 [:atk 0.01 :dur 1 :rq 0.7])
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





(def cs3
  {:motif1 (p/scale-p
            bass2
            :D4 :major
            [:3<<< :| :|
             :5<<< :| :|
             :1<< :| :|
             :7<<< :| :|]
            1/8 0 [:atk 1 :f-dur 0.5 :echo 0 :decay 2 :amp 1.0 :cutoff 2000 :cutoff2 1000 ])

   :drum3 (drum-p2
           [:Kit15-Electro]
           [ :| :cl1 :|]
           1/4)
   :drum2 (drum-p2
           [:Kit5-Electro]
           [(fn [d n]
              (cond (= n 1) (choose [:k1 :k2])
                    (odd? n) (choose [:fx1 :fx3 :s1 :hf nil])
                    true (p/w-choose {:s1 0.5 nil 0.5}))
              ) :|]
           1/4)
   :harmony1 (merge
              (p/scale-p
               cs80
               :D4 :major
               [:1>                        :|
                :|
                :5>                        :|
                :|
                :02 :3>                    :|
                :|
                :07 :7 [:dur 4]            :| :| :| :| :| :|
                :4>                        :|
                :|
                :3>                        :|
                :|
                :02 :3>                    :|
                :|
                :07 [:7 :1 :5 :3 {:dur 4}] :| :| :| :| :| :|]
               1/8 0 [:amp 0.2 :dur 3.0 :atk 0.3 :rq 0.5 :cutoff 3000 :dtune 0.002 :vibrate 1.0 :vibdepth 0 :freq-lag 0.1 ])
                                        ;{:fx {:shift [p/p-pitch-shift :pitch-ratio 1.5]}}
              {:p-size [12 8]}
              )
   :motif2 (p/scale-p
            bass2
            :D4 :major
            [:1< :01 :1< :03 :7<< :|
             :02 :3< :02 :5< :|
             :3< :01 (fn [d] (choose [:4< :4 :3])) :03 :1< :| :|]
            1/8 0 [:atk 0.001 :f-dur 0.5
                   :echo 0 :decay 1.3 :amp 1.0 :cutoff 2000 :cutoff2 5000])
   :motif3 (p/scale-p
            bass-synth
            :A4 :major
            [:3 :04 :2        :|
             :03 :1           :|
             :7< :04 :6<      :|
             :02 :7<          :|
             :1 :05 :2        :|
             :01 :3 :03 :3    :|
             :01 :3 :03 :3    :|
             :3 :01 :4 :04 :3 :| :| :| :| :|]
            1/8 0 [:attack 0.01 :amp 0.3 :atk 0.01 :dur 1.4 :release 1 :detune 0 :bwr 1.4 ])

   })


(def pads
  {:piano (p/scale-p
           piano
           :D4 :major
           [:5<< :02 :1< :02 :1     :|
            :01 :5 :05 [:7< :2<]    :|
            :|
            :02 [:5< :1< :7<]       :|
            :05 :1                  :|
            :03 :2                  :|
            :01 [:7< :2< :5<]       :| :| :|
            :| :| :|
            :04 [:7 :7< :5<] :02 :6 :|
            :02 :5 :02 :3  :05  :4  :|
            :01 [:3 :5< :2<]        :| :|
            [:4< :1< :7<]           :|
            :01 :1 :05  :2          :| :|
            [:7< :5 :2 ]            :| :| :| :|
            ]
           1/8 0 [:amp 0.4 :dur 5 :vel 100.0 :decay 0.8 :release 0.8 :hard 0.3 :velhard 0.8 :muffle 3 :velmuff 0.8 :velcurve 0.8 :stereo 0.2 :tune 0.5 :random 0.1 :stretch 0.1 :sustain 0.5 ])
   :perc (merge
          (drum-p2
           [:Shakers :claves]
           [(fn [d] (choose [:shaker1 :shaker2 :shaker4 :shaker6 :hit4 :hit5 :hit1])) :01 :| :|
            :| :|]
           1/4)
          {:fx {:delay [p-delay :max-delay 1 :delay 0.2 :decay 5]
                ;:shift [p/p-pitch-shift :pitch-ratio 4 :pitch-dispersion 1.4]
                }})
   :bell (merge
            (p/scale-p
             reverb-test
             :D4 :major
             [:5<< :04 :1    :|
              :02 :7<        :|
              :6< :04 :4<<   :|
              :02 :2 :04 :7< :|
              :04 :5<        :|
              :02 :3<<       :|
              :6<  :04 :3<   :|
              :02 :5<        :| :| :| :| :|
              ]
             1/8 0 [:max-delay 0.5 :delay-time 0.3 :decay 3 :amp 0.5 ])
            )
   :saw  (p/scale-p
               bpfsaw2
               :D4 :major
               [[:7 :7] :01 [:7 :7] :01 [:7 :7] :02 [:7 :7]           :|
                :01 [:7 :7] :01 [:7 :7] :02 [:7 :7]                   :|
                [:7 :7] :04 [:6 :6]                                   :|
                [:6 :6] :01 [:6 :6] :02 [:6 :6] :01 [:6 :6]           :|
                :01 [:6 :6]                                           :|
                :02 [:5 :5] :01 [:5 :5] :02 [:5 :5]                   :|
                :02 [:5 :5] :01 [:5 :5] :01 [:5 :5]                   :|
                :06 [:3 :3]                                           :|
                [:3 :3] :01 [:3 :3] :01 [:3 :3] :01 [:3 :3]           :|
                :|
                :|
                :|
                :05 [:7 :7] :01 [:7 :7]                               :|
                :02 [:7 :7] :01 [:7 :7] :01 [:7 :7]                   :|
                :01 [:7 :7] :01 [:7 :7] :02 [:7 :7]                   :|
                [:7 :7] :05 [:6 :6]                                   :|
                :01 [:6 :6] :01 [:6 :6] :01 [:6 :6] :01 [:6 :6]       :|
                :|
                :04 [:4> :4> :1> :1>] :01 [:4> :4> :1> :1>]           :|
                :01 [:4> :4> :1> :1>] :02 [:1> :1>] :01 [:4> :4>]     :|
                :|
                [:7 :7] :02 [:7 :7 :3> :3>] :01 [:7 :7] :01 [:3> :3>] :|
                [:7 :7] :01 [:3> :3>]                                 :| :|]
               1/8 0 [:atk 2.0 :sus 0.0 :rel 3.0 :c1 1.0 :c2 -1.0 :detune 0.2 :pan 0.0 :cfhzmin 0.1 :cfhzmax 0.3 :cfmin 500.0 :cfmax 2000.0 :rqmin 0.1 :rqmax 0.2 :lsf 200.0 :ldb 0.0 :hsf 6000.0 :hdb 0.0 :amp 0.3 :rs 0.5 ])
   :cs80  (p/scale-p
               cs80
               :D4 :major
               [[:7 :7] :01 [:7 :7] :01 [:7 :7] :02 [:7 :7]           :|
                :01 [:7 :7] :01 [:7 :7] :02 [:7 :7]                   :|
                [:7 :7] :04 [:6 :6]                                   :|
                [:6 :6] :01 [:6 :6] :02 [:6 :6] :01 [:6 :6]           :|
                :01 [:6 :6]                                           :|
                :02 [:5 :5] :01 [:5 :5] :02 [:5 :5]                   :|
                :02 [:5 :5] :01 [:5 :5] :01 [:5 :5]                   :|
                :06 [:3 :3]                                           :|
                [:3 :3] :01 [:3 :3] :01 [:3 :3] :01 [:3 :3]           :|
                :|
                :|
                :|
                :05 [:7 :7] :01 [:7 :7]                               :|
                :02 [:7 :7] :01 [:7 :7] :01 [:7 :7]                   :|
                :01 [:7 :7] :01 [:7 :7] :02 [:7 :7]                   :|
                [:7 :7] :05 [:6 :6]                                   :|
                :01 [:6 :6] :01 [:6 :6] :01 [:6 :6] :01 [:6 :6]       :|
                :|
                :04 [:4> :4> :1> :1>] :01 [:4> :4> :1> :1>]           :|
                :01 [:4> :4> :1> :1>] :02 [:1> :1>] :01 [:4> :4>]     :|
                :|
                [:7 :7] :02 [:7 :7 :3> :3>] :01 [:7 :7] :01 [:3> :3>] :|
                [:7 :7] :01 [:3> :3>]                                 :| :|]
               1/8 0 [:amp 0.01 :dur 3.0 :atk 0.3 :rq 0.5 :cutoff 6000 :dtune 0.002 :vibrate 4.0 :vibdepth 0.015 :freq-lag 0.1 ])
   :pad (merge
              (p/scale-p
               overpad2
               :D4 :major
               [[:7< :5<]               :|
                :03 [:7< :5<]           :|
                [:5< :7<] :05 [:4< :6<] :|
                :|
                :01 [:6< :4<]           :|
                :06 [:3< :2]            :|
                :|
                :01 [:3< :1]            :|
                :05 [:7< :5<]           :| :| :| :|]
               1/8 0 [:amp 0.4 :attack 2 :release 2.0 ])
              {:fx {:low-pass [p/p-low-shelf :freq 500 :db -18]
                    :peak [p/p-peak-eq :freq 120 :db -18]
                    }})

   })

(def summer
  {:tempo 100
   :motif2 (p/scale-p
            bass-synth
            :G4 :major
            [(fn [d] (choose [:7< [:7< :5<]])) :03 :1 :|
             :04 (fn [d] (choose [:3 [:3 :1]]))       :|
             :04 :1                                  :|
             :04 :6<                                 :|
             :04 (fn [d] (choose [:1 [:4< :1]]))      :| :| :| :|]
            1/8 0 [:attack 0.01 :amp 0.5 :release 2 :detune 0 :bwr 1.0 ])

   :motif1 (p/scale-p
            plk-bass
            :G4 :major
            [:5<< :05 :7<<< :|
             :04 :5<<       :|
             :04 :2<<       :|
             :4<< :03 :3<<  :|
             :1<< :05 :2<<  :| :| :| :|]
            1/8 0 [:dur 0.5 :amp 0.5 :plk 2])

   :harmony1 (p/scale-p
              klang-test
              :G4 :major
              [[:5 :7] :05 [:3 :5] :|
               :02 [:7 :5]         :|
               [:3 :5] :03 [:7 :5] :|
               :04 [:6 :4]         :|
               :04 [:6 :4]         :| [:2 :4] :| :| :|]
              1/8 0 [:amp 0.2 :atk 0.01 :dur 2 ])

   :drum1 (drum-p2
           [:Kit13-Acoustic]
           [:k1 :5 :k2 :| :s1 :|
            ]
           1/8)
   :drum2 (drum-p2
           [:Bongos :Congas]
           [:bongo4  :1 :bongo4 :3 :bongo5 :| :4 :Conga10 :|]
           1/8)

   })

(def eastern
  {:string (p/scale-p
            ks2
            :G4 :major
            [
             [:2< :2<<] :02 :3b< :02 :6< :|
             :01 :5< :02 [:3b< :1<<]    :|
             :2< :03 :1<                :|
             :02 [:7<< :7<<<]            :|
             :01 [:7<<< :7<<] :03 :1<    :|
             :2< :06 [:2< :2<<]          :| :|
             ]
            1/8 0 [:amp 0.8 :dur 2.0 :decay 30.0 :coef 0.3 ])
   :asd {}})
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
        [:c1 [:amp 0.5] :1]
        1/4)

   :saw (p/phrase-p
         bpfsaw2
         [[:E4 :E3] :| :| :| [:A3 :A4] :| :| :| [:D4 :D3] :| :|]
         1/8 0 [:t 3 :detune 0 :hsf 5000 :hdb -16])

   :kicks  (let [c1 [(drum-s [:Kit7-Electro] :c1) [:amp 0.5]]
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
         (p/scale-p
          zap2
          :C4 :major
          [(fn [d] [(choose [:2> :4> :4 :6 :6> :3 :3> nil])
                   [:dur (max 0.3 (rand 0.7))]]) :01 :| :| :| :|]
          1/16 0 [:dur 0.4 :amp 2 :sig2 1 :mul 0.5 ])
         :fx {:delay [p/p-delay :max-delay 2 :delay 0.2 :decay 6]
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
(def tekno1
  {:tempo 130
   :motif3 (p/scale-p
            cs80
            :C4 :major
            [[:2b> :2b>] :06 [:2b> :2b>] :07 [:2b> :2b>] :|
             :07 [:2b> :2b>]                             :|
             :01 [:2b> :2b>]                             :|
             :06 [:2b> :2b>]                             :|
             :|
             :|
             :|
             :012 [:2> :2>]                              :|
             :05 [:2> :2>] :08 [:2> :2>]                 :|
             :08 [:2> :2>]                               :|
             [:2> :2>] :07 [:2> :2>]                     :|
             :|
             :|
             :|
             :|
             :014 [:5b> :5b>]                            :|
             :06 [:5b> :5b>] :06 [:5b> :5b>]             :|
             :04 [:5b> :5b>] :07 [:5b> :5b>]             :|
             :04 [:5b> :5b>] :08 [:5b> :5b>]             :|
             :|
             :|
             :|
             :|
             :011 [:2b> :2b>]                            :|
             :05 [:2b> :2b>] :06 [:2b> :2b>]             :|
             :05 [:2b> :2b>] :08 [:2b> :2b>]             :|
             :08 [:2b> :2b>]                             :|
             :02 [:2b> :2b>]                             :|]
            1/16 0 [:amp 0.04 :dur 3.0 :atk 0.3 :rq 0.5 :cutoff 5000 :dtune 0.002 :vibrate 4.0 :vibdepth 0.015 :freq-lag 0.1 ])
   :motif2 (p/scale-p
            harmonic
            :C2 :major
            [[:2b> :2b>] :06 [:2b> :2b>] :07 [:2b> :2b>] :|
             :07 [:2b> :2b>]                             :|
             :01 [:2b> :2b>]                             :|
             :06 [:2b> :2b>]                             :|
             :|
             :|
             :|
             :012 [:2> :2>]                              :|
             :05 [:2> :2>] :08 [:2> :2>]                 :|
             :08 [:2> :2>]                               :|
             [:2> :2>] :07 [:2> :2>]                     :|
             :|
             :|
             :|
             :|
             :014 [:5b> :5b>]                            :|
             :06 [:5b> :5b>] :06 [:5b> :5b>]             :|
             :04 [:5b> :5b>] :07 [:5b> :5b>]             :|
             :04 [:5b> :5b>] :08 [:5b> :5b>]             :|
             :|
             :|
             :|
             :|
             :011 [:2b> :2b>]                            :|
             :05 [:2b> :2b>] :06 [:2b> :2b>]             :|
             :05 [:2b> :2b>] :08 [:2b> :2b>]             :|
             :08 [:2b> :2b>]                             :|
             :02 [:2b> :2b>]                             :|]
            1/16 0 [:amp 0.1 :dur 4 ])

   :motif1 (p/scale-p
            bass2
            :C4 :major
            [:5<<< :011 :2b<< :|
             :09 :5<<         :|
             :08 :5b<<        :|
             :2b<<            :|]
            1/16 0 [:atk 0.2 :f-dur 0.6 :echo 1.0 :decay 1.4 :amp 1.0 :cutoff 5000 :cutoff2 2000.0 ])

   :drum4 (drum-p2
           [:Kit5-Electro]
           [(fn [d n] (if (odd? n) (choose [:c1 :c2 nil]))) :|]
           1/8)
   :drum3 (drum-p2
           [:Kit17-Electro]
           [:| :s2 :|]
           1/4)
   :drum2 (drum-p2
           [:Kit6-Electro]
           [:2 :o1 :|]
           1/4)
   :drum1 (drum-p2
           [:Kit6-Electro]
           [:k1                     :|
            :k1                     :|
            :k1 :1 :c2 :1 :o :1 :s1 :|
            :k1 :03 :s1             :|]
           1/8)

   })

(def tekno2
  {:tempo 120
   :motif1 (p/scale-p
            acid-bass
            :C4 :major
            [:2<< :05 :2<<           :|
             :04 :2<<                :|
             :02 :2<<                :|
             :2<< :01 :4<<< :01 :3<< :|
             ]
            1/8 0 [:amp 0.15 :lg 1.0 :dur 0.25 :attack 0.001 :reverb 1.0 ])

   :harmony1 (p/scale-p
              fmTest
              :C4 :major
              [
               [:1> :2 :4<]                                    :|
               :|
               :|
               :01 [:1> [:gate 0]] [:2 [:gate 0]] [:4< [:gate 0]]  :|
               [:7< :2< :5]                                       :|
               :|
               :|
               [:5 [:gate 0] :2< [:gate 0] :7< [:gate 0]]          :|
               [:3 :7<< :5<]                                       :|
               :|
               :|
               :|
               :|
               :02 [:3 [:gate 0]] [:5< [:gate 0]] [:7<< [:gate 0]] :| :| :|]
              1/8 0 [:gate 1.0 :amp 1.0 :outBus 0.0 ])

   :drum3 (drum-p2
           [:Kit15-Electro]
           [:| :cl1 :|]
           1/4)
   :drum2 (drum-p2
           [:Kit15-Electro]
           [                                                                                                            :|
            :02 [(drum-s [:Kit15-Electro] :o2) []]                                                                      :|
            [(drum-s [:Kit15-Electro] :o2) []] [(drum-s [:Kit15-Electro] :o2) []] :1 [(drum-s [:Kit15-Electro] :o1) []] :|
            :|
            ]
           1/4)
   :drum1 (drum-p2
           [:Kit15-Electro]
           [[(drum-s [:Kit5-Electro] :k2) []]                                                                        :|
            [(drum-s [:Kit5-Electro] :k2) []] [(drum-s [:Kit5-Electro] :c2) []] [(drum-s [:Kit5-Electro] :c2) []]    :|
            [(drum-s [:Kit5-Electro] :k2) []] :1 [(drum-s [:Kit5-Electro] :c2) []]                                   :|
            [(drum-s [:Kit5-Electro] :k2) []] [(drum-s [:Kit5-Electro] :c2) []] :1 [(drum-s [:Kit5-Electro] :c2) []] :|
            ]
           1/4)
   :lead (p/scale-p
          bass2
          :C4 :major
          [:3> :05 :6                               :|
           :04 :7                                   :|
           :02 :1>                                  :|
           :2>                                      :|
           :3> :05 :2>                              :|
           :04 :1>                                  :|
           :02 :7                                   :|
           :5                                       :|
           :6 :03 :7                                :|
           :04 :1>                                  :|
           :04 :7                                   :|
           (fn [d n] (if (= n 1) (choose [:5 :5>]))) :|
           :|
           :|
           :|
           :|
           ]
          1/8 0 [:atk 0.001 :f-dur 0.001 :echo 0 :decay 1.4 :amp 1.0 :cutoff 2000.0 :cutoff2 2000.0 ])
   :arp (p/scale-p
         bing
         :C4 :major
         [:2 :5  :7  :3>  :|
          :2  :5  :7  :3> :|
          :2  :5  :7  :3> :|
          :2  :5  :7  :3> :|
          :1  :5  :7  :3> :|
          :1  :5  :7  :3> :|
          :1  :5  :7  :3> :|
          :1  :5  :7  :3> :|
          :3  :5 :7  :1>  :|
          :3 :5 :7  :1>   :|
          :3 :5 :7  :1>   :|
          :3 :5 :7  :1>   :|
          :3 :5 :7  :1>   :|
          :3 :5 :7  :1>   :|
          :3 :5 :7  :1>   :|
          :3 :5 :7  :1>   :|
          ]
         1/4 0 [:decay 0.8 :attack 0.06 :amp 0.6])
   :kick (drum-p2
          [:Kit4-Electro]
          [[(drum-s [:Kit6-Electro] :k2) []] :|])
   :cl (drum-p2
        [:Kit6-Electro]
        [:c1]
        1/4)
   })


(def pads2
  {:harmony1 (p/scale-p
              vibPad
              :C4 :major
              [[:6< :3 :4<]                                     :|
               :|
               :|
               :|
               :06 [:6< [:gate 0]] [:4< [:gate 0] :3 [:gate 0]] :|
               :03 [:3< :2 :5<]                                 :|
               :|
               :|
               :|
               :03 [:3< [:gate 0] :2 [:gate 0] :5< [:gate 0]]   :02 [:1 :2< :4<] :|
               :|
               :|
               :|
               :|
               :|
               :06 [:1 [:gate 0] :2< [:gate 0] :4< [:gate 0]]   :|]
              1/8 0 [:gate 1.0 :outBus 0.0 :amp 0.05 ])

   :harmony2 (p/scale-p
              sawPad
              :C4 :major
              [[:2> :5>]                                          :|
               :03 [:1> :6> :2> [:gate 0] :5> [:gate 0]]          :|
               :4 :04 [:4 [:gate 0] :3]                           :|
               :|
               [:3 [:gate 0] :5> :1> [:gate 0] :2> :6> [:gate 0]] :|
               :03 [:3 :3>] [:2> [:gate 0] :5> [:gate 0]]         :|
               :06 :4                                             :|
               :02 [:3 [:gate 0]] :5 :01 [:4 [:gate 0]]           :|
               :01 [:5 [:gate 0] :6] [:3> [:gate 0] :1>]          :|
               :05 [:4 :2>] [:6 [:gate 0] :1> [:gate 0]]          :|
               :|
               [:2> [:gate 0] :5> :1>] [:4 [:gate 0]]             :|
               :|
               :|
               :05 [:5> [:gate 0] :1> [:gate 0]] :|  :|]
              1/8 0 [:gate 1.0 :outBus 0.0 :amp 0.1 ])


   })
(def tekno3
  {:tempo 120
   :motif2 (p/scale-p
            bass-synth
            :C4 :major
            [:5>         :|
             :1> :04 :2> :|
             :04 :3>     :|
             :04 :1>     :|
             :6          :|
             :|
             :|
             :|
             :5>         :|
             :1> :03 :2> :|
             :04 :3>     :|
             :04 :1>     :|
             :04 :7      :|
             :|
             :|
             :|
             ]
            1/8 0 [:attack 0.001 :amp 1.0 :release 1.0 :detune 3.0 :bwr 2.0 ])

   :motif1 (p/scale-p
            bass2
            :C4 :major
            [:4<< :|
             :|
             :|
             :|
             :3<< :|
             :|
             :|
             :|
             :6<< :|
             :|
             :|
             :|
             :2<< :|
             :|
             :|
             :|
             ]
            1/8 0 [:atk 1 :f-dur 0.4 :echo 0 :decay 2.6 :amp 0.4 :cutoff 2000.0 :cutoff2 1000 ])

   :harmony1 (p/scale-p
              cs80
              :C4 :major
              [:6 :02 :6 :03 :6 :|
               :|
               :01 :6           :|
               :07 :7           :|
               :03 :7 :02 :7    :|
               :01 :7           :|
               :|
               :|
               :01 :1> :02 :1>  :|
               :1>              :|
               :1>              :|
               :|
               :3> :03 :3>      :|
               :3> :03 :3>      :|
               :|
               :|
               :01 :6 :04 :6    :|
               :02 :6 :03 :6    :|
               :02 :6           :|
               :|
               :7 :03 :7 :02 :7 :|
               :|
               :|
               :|
               :1> :03 :1>      :|
               :1>              :|
               :|
               :06 :5>          :|
               :02 :5> :02 :5>  :| :| :| :|]
              1/8 0 [:amp 0.4 :dur 2.6 :atk 0.3 :rq 0.5 :cutoff 7000.0 :dtune 0.002 :vibrate 4.0 :vibdepth 0.015 :freq-lag 0.1 ])

   :drum1 (drum-p2
           [:Kit15-Electro]
           [[(drum-s [:KurzweilKit08] :kick1) []]                                                                                :|
            [(drum-s [:KurzweilKit08] :kick1) []]                                                                                :|
            [(drum-s [:KurzweilKit08] :kick1) []] [(drum-s [:KurzweilKit08] :perc8) []] :1 [(drum-s [:KurzweilKit08] :perc8) []] :|
            [(drum-s [:KurzweilKit08] :kick1) []] :1 [(drum-s [:KurzweilKit08] :sdst2) []]                                       :|
            ]
           1/4)
   :drum4 (drum-p2
           [:Kit15-Electro]
           [:2 [(drum-s [:Kit17-Electro] :ophat) []] :|]
           1/4)
   :drum3 (drum-p2
           [:Kit15-Electro]
           [:| :cl1 :|]
           1/4)
   :drum2 (drum-p2
           [:Kit15-Electro]
           [[(drum-s [:KurzweilKit08] :pdhat) []] [(drum-s [:KurzweilKit08] :pdhat) []]                                    :|
            :02 [(drum-s [:KurzweilKit08] :pdhat) []]                                                                      :|
            [(drum-s [:KurzweilKit08] :tom2) []] [(drum-s [:KurzweilKit08] :tom3) []] [(drum-s [:KurzweilKit08] :tom2) []] :|
            :02 [(drum-s [:KurzweilKit08] :tom2) []]                                                                       :|
            ]
           1/4)

   })
