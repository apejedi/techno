(ns techno.motifs
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.synths])
  )


(defonce motif (atom (fn [_])))
(swap! motif (fn [_]
               (fn [b]
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
               ))

(def ambient (fn [b]
               (let [n (choose (scale :C5 :minor))
                     dur (s/get-rand-int 1 3)]
                   (if  (= (- b (int b)) 0.5)
                     [bpfsaw [n :dur dur]
                      ])
                   )
               ))
(defonce chords (atom []))

(swap! chords
       (fn [_]
         (let [get-p (fn [d]
                       (s/chord-p sweet
                                  (chord-degree d :C4 :minor 4)
                                  [:amp 0.2 :dur 1 :coef 0.01 :attack 1 :release 2])
                       )
               prog {1 (get-p :vi)
                     2 (get-p :v)
                     3 (get-p :iv)
                     4 (get-p :ii)
                     4.75 []
                     }]
           prog
           )
         ))

(defonce arpeggio (atom nil))
(swap! arpeggio (fn [_]
                  (let [root :C4
                        type :minor
                        args [:coef 0.001 :amp 0.4 :atk 0.01 :dur 1]
                        inst ks1
                        v (flatten (repeat 8 (chord-degree :v root type 4)))
                        i (flatten (repeat 8 (chord-degree :iv root type 4)))]
                    (s/arp-p inst (concat v i) args 0)
                    )
                  ))

(def scatterbrain
  (let [inst ks1
        args [:coef 0.001 :dur 2 :atk 0.001 :amp 0.7]
        t [:dur 2]
        s [:space 1]
        main [:C3 :A4 :E5 t s
              :B2 :D4 :E5 t s
              :A3 :F4 :C5 t s
              :D5 :E5 :B4 t s
              ;:G3 :E4 :B4 t s
              ]
        switch [:F#3 :Eb4 :A4 t s
                :E3 :E4 :G4 t s
                :E3 :Eb4 :A4 s
                :B4 s]
        ;; main [:G4 :A4 :E5 t s
        ;;       :F#4 :A4 :D5 t s
        ;;       :E4 :G4 :C5 t s
        ;;       :D4 :F#4 :B4 t s]
        ;; switch [:E4 :F#4 :A4 t s
        ;;         :E4 :F#4 :G4 t s
        ;;         :E4 :F#4 :A4 t s
        ;;         :B4 s]
        switch2 [:G4 :B4 :Eb5 t s
                 :D5 t s
                 :C5 t s
                 :F5 t s]]

    (s/phrase-p
     inst
     (concat
      main
      main
      switch
      ;switch2
      )
     (double (/ 1 4))
     0
     args
     )
    ))


(def coffee (atom nil))
(swap! coffee
       (fn [_]
         (let []
             (s/phrase-p
              piano
              [
               [:F#5 :A5 :F#6]
               [:F#5 :B6]
               [:F#5 :C#6]
               ;; [:F#4 :A4 :F#5] :F4
               ;; [:Ab4 :B4 :Ab5] :F4
               ;; [:A4 :C#4 :A5] :F4
               ;; [:F4 :A4 :F5] :F4
               ]
              (double (/ 1 4))
              0
              [:dur 2 :amp 0.7 :coef 0.01]
              ))
         ))

(defn rnd-chord [b]
  (if (weighted-coin 0.8)
    (s/chord-p overpad
               (chord-degree
                (choose [:i :iv :v :vi])
                :C4
                (choose [:minor]) 4)
                [:coef 0.01 :amp 0.2 :dur 2 :attack 0.1 :release 2]
                )
    ))
(comment
  (s/add-p core/player scatterbrain :sc)
  (s/add-p core/player ambient :harmony)
  (s/add-p core/player melissa :harmony)
  (do
    (s/add-p core/player melissa-motif :motif)
    (s/add-p core/player techno.drums/melissa-b :main)
    )
  (s/add-p core/player motif :motif)
  (s/add-p core/player coffee :harmony)
  (s/add-p core/player untitled :harmony)
  (s/add-p core/player untitled-f :motif)
  (s/add-p core/player rnd-chord :harmony)

  (s/play-p acid ;core/player acid :acid ;{:sc303 1001}
            5)
  (ctl t :gate 0)
  (ctl t :gate 1 :freq 100 :dec 4 :sus 1 :wave 0)

  (s/rm-p core/player :harmony)
  (s/add-p core/player chords :harmony)
  (s/add-p core/player arpeggio :arp)
  (s/rm-p core/player :harmony)
  (s/wrap-p core/player :harmony true)
  (s/add-p core/player ted-guitar :guitar)
  (s/add-p core/player x-naut :x-naut)
  (s/add-p core/player [(s/chord-p overpad [:F#4 :C#5 :Eb5 :Bb4] [:attack 1 :release 3]) nil nil nil nil nil])

  (s/add-p
   core/player
   (s/phrase-p
    rise-fall-pad
    [(map midi->hz (chord :C3 :M7)) (map midi->hz (chord :F3 :M7)) :34]
    0.25 32 [:t 5])
   :harmony)

  (s/add-p
   core/player
   (s/phrase-p
    bass-synth
    [[:E3 :B3] [:A3 :F3] :34]
    0.25 32 [:release 6 :amp 0.4 :detune 4])
   :motif)



  (s/add-p
   core/player
   (let [a [:D3 :E4] b [:D3 :D4] l [:dur 2 :coef 0.01 :amp 0.6]
         a1 [:D3 l :E4 l] b1 [:D3 l :D4 l]]
       (s/phrase-p
        ks1
        [a1 :3 a a a :1 b1 :3 b b b :3]
        0.25 1 [:coef 0.01 :amp 0.5]))
   :motif)

  (s/play-p
   ;core/player
   (let [root :D4
         n 3
         [a b c d e] (map #(chord-degree % :C4 :minor) [:i :ii :iii :iv :v])]
     (s/phrase-p
      bing
      [b d c e a]
      0.25 2
      [:decay 2 :release 1 :dur 2 :amp 0.3 :coef 0.01]
      {:refresh 0.6 :sputter 0.5 :sputter-amt 0.25 :reverse 0}))
   1
   )
  (s/add-p
   core/player
   (let [a (chord :C4 :M7)
         b (chord :B3 :M7)]
     (s/phrase-p
      bass-synth
      [a a a a a :0 b b b b b :1]
      0.25 2 [:dur 0.8 :amp 0.5 :vib 0]))
   :harmony
   )
  (s/play-p
   (s/phrase-p
    mooger
    [:D3 :Eb3 :D3 :Bb2 :C3 [:t 2] :10]
    0.25 3 [:amp 1]) 1.3)
  (s/play-p
   ;core/player
   (let [a [:D3 :Eb4 :F#5]
         b [:D3 :Eb4 :G5]
         c [:D3 :Eb4 :A5]
         d [:D3 :Eb4 :Bb5]
         e [:D3 :F#4 :A5]]
     (s/phrase-p
      ks1
      (concat a a a a b b b b
              c c c c d d d d
              e e e e)
      0.25 0 [:coef 0.01 :amp 1 :dur 1.3]))
   ;:harmony2
   )
  (kill trigger-synth)
  (s/rm-p core/player :harmony)
  )




(def untitled
  (let [[c b d a] (map #(vector (note %) (- (note %) 4)) [:C4 :Bb3 :D4 :A3])
        -- [:space 2]
        | [:space 0]
        phrase [c -- c -- c c
                b -- b | b -- b  b  b
                d -- d -- d  d
                a -- a | a -- a  a  a]]
    (s/phrase-p
     piano
     phrase
     (double (/ 1 4))
     1
     [:dur 2 :amp 0.3]))
  )
(def untitled-f
  (let []
    (s/phrase-p
     flute
     [:D4 :Eb4 :F4 :G4 :F#4 :Ab4 :Bb4 [:space 3]]
     (double (/ 1 4))
     6
     [:dur 1.5]
     )
    ))

(def ted-guitar
  (let [inst piano
        args [:coef 0.01 :dur 2]]
    (s/phrase-p
     inst
     [(chord :G4 :m7)
      (chord :F#3 :M7)]
     0.25
     2
     args
     )
    )
  )



(def acid
  (let [inst sc303
        f #(vector [inst [:freq % :gate 1]]
                   [inst [:gate 0]])
        freqs [65.40639132515
               77.78174593052
               48.999429497719
               73.416191979352
               65.40639132515
               65.40639132515
               130.8127826503
               65.40639132515
               65.40639132515
               32.703195662575
               65.40639132515
               65.40639132515
               65.40639132515
               32.703195662575
               65.40639132515]]
    (into [[sc303 [:freq 65.406395 :env 5000.0 :sus 2 :ctf 1294.8276 :res 1.0 :dec 1.4827586 :wave 1.0 :vol 0.2]]]
          (mapcat f freqs))
    )
  )
