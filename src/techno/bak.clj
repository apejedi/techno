(ns techno.bak)

(def sketch
  {:harmony1   (s/phrase-p
                prophet
                [[:Bb3 :D4 :G4] :31 [:Bb4 :Eb4 :C4] :17 [:G3 :G4]  :17 [:F3 :Eb4 :C4] :27
                 ]
                0.25 0 [:rq 0.9 :cutoff 4000 :attack 2 :decay 2.4])
   :shkr  (s/fit-p {1.75 []} (drum-p [:Kit8-Vinyl] [:shkr3 :shkr3 :shkr1 :1]))
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
   :drum1   (s/build-map-p
             [:4
              [ dirty-kick [:amp 0.5]   ] [ o-hat []   ] [ o-hat []   ] :1
              [ (get-in drum-kits [:Kit4-Electro :CYCdh_ElecK01-Kick02.wav]) []   ] :1
              [ o-snr []   ] :5

              ])
   :bass2 (s/phrase-p
           bass2
           [:C5 :2 :G5 :4 :Ab5 :3 :Bb5 :6 :Ab5 :5 :G5 :5 :D5 :6 :G5 :9]
           0.25 0 [:amp 8.5 :decay 1.6 :cutoff2 5196])

   :claves (s/build-map-p
            [:2
             [ (get-in drum-kits [:claves :121391__soundbytez__claves-hit09.wav]) []   ] :1
             [ (get-in drum-kits [:claves :121383__soundbytez__claves-hit01.wav]) []   ] :1
             [ (get-in drum-kits [:claves :121387__soundbytez__claves-hit05.wav]) []   ] [ (get-in drum-kits [:claves :121385__soundbytez__claves-hit03.wav]) []   ] :2
             [ (get-in drum-kits [:claves :121378__soundbytez__claves-decres.wav]) []   ] :1
             [ (get-in drum-kits [:claves :121384__soundbytez__claves-hit02.wav]) []   ] :3

             ])
   :motif1 (s/phrase-p
            bass-synth
            [:Bb3 :1 :G3 :5 :D3 :6 :C3 :16 :Bb3 :1 :G3 :5 :D3 :6 :Eb3 :16]
            0.25 0 [:release 2.2 :attack 0.1 :bwr 2.3])
   :arp (s/phrase-p
         prophet
         [:C5 :Eb5  :G5  :C5  :Eb5 :G5  :C5 :Eb5  :G5  :C5  :Eb5  :G5  :C5 :Eb5  :G5  :Bb4  :D5  :G5  :Bb4  :D5  :G5  :Bb4  :D5  :G5  :Bb4  :D5  :G5 :Bb4 :G4 :Bb4  :D5  :G4  :Bb4  :D5  :G4 :Bb4  :D5  :G4  :Bb4 :D5  :D5 :3 :F4 :Ab4  :C5  :F4  :Ab4  :C5  :F4  :Ab4  :C5  :F4  :Ab4 [:decay 1.3 :attack 0.7] :7]
         0.25 1 [:attack 0.07 :decay 0.9 :amp 1.4])
   :motif3 (fn
             ([] [4.75 0.125])
             ([b]
              (let [cfmin  (choose (map midi->hz (scale :C5 :minor)))
                                        ; cfmin (* (midi->hz 64) (choose [0.5 1 2 4]))
                    ]
                (if (= (rand-int 2) 1)
                  [piano
                   [:dur (choose [3 1.5])
                    :atk 0.01
                    :note (choose (scale :C5 :minor))
                    :rq 0.4
                    :amp 0.8]]
                  )))
             )
   :drum4  (s/build-map-p
            [[ (get-in drum-kits [:KurzweilKit04 :CYCdh_Kurz04-Snr03.wav]) []   ] [ b-snr []   ] :1
             [ (get-in drum-kits [:KurzweilKit04 :CYCdh_Kurz04-Tom02.wav]) []   ] :2
             [ (get-in drum-kits [:KurzweilKit04 :CYCdh_Kurz04-Tom03.wav]) []   ] :1
             [ (get-in drum-kits [:KurzweilKit04 :CYCdh_Kurz04-Snr03.wav]) []   ] [ b-snr []   ] :1
             [ (get-in drum-kits [:KurzweilKit04 :CYCdh_Kurz04-Tom02.wav]) []   ] :2
             [ (get-in drum-kits [:KurzweilKit04 :CYCdh_Kurz04-Tom03.wav]) []   ] :1

             ])
   :harmony2   (s/phrase-p
                bpfsaw2
                [:C4 :C4 :1 :C4 :1 :C4 :1 :C4 :1 :C4 :1 :C4 :1 :C4 :1 :C4 :1 :C4 :11 :D4 :1 :D4 :D4 :1 :D4 :D4 :10 :Eb4 :1 :Eb4 :1 :Eb4 :1 :Eb4 :Eb4 :1 :Eb4 :1 :Eb4 :11 :Bb4 :1 :Bb4 :1 :Bb4 :1 :Bb4 :Bb4 :1 :Bb4 :1 :Bb4 :1 :Bb4 :Bb4 :1 :Bb4 :1 :Bb4 :8 :G4 :1 :G4 :1 :G4 :1 :G4 :1 :G4 :G4 :1 :G4 :G4 :1 :G4 :1 :G4 :G4 :14]
                0.25 0 [])
   :drum2 (drum-p [:Kit15-Electro]
                  [:4 :cl1 :3] 0.25 )
   :kick (drum-p [:Kit15-Electro]
                 [
                  [o-kick []]
                  :3
                  ])

   :drum3  (s/build-map-p
            [:2
             [ (get-in drum-kits [:KurzweilKit04 :CYCdh_Kurz04-Ride01.wav]) []   ] :1

             ])

   })
