(ns techno.motifs
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.synths]
        [techno.melody])
  )

(defonce motif (atom (fn [_])))
(swap! motif (fn [_]
               (s/phrase-p
                ks1
                [:Bb5 :Ab5 :G5 :Ab5 :G5 [:dur 5] :3 :D4 :Eb4 :D4 :1 :C4 [:dur 8] :3]
                0.25 0 [:coef 0.01 :dur 2]
                {:refresh 0.7 :sputter 0.5 :sputter-amt 0.2}
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
         (let [c #(chord-degree % :C4 :minor 4)]
        (s/phrase-p
         sweet
         [(c :v) (c :ii) (c :vi) (c :i) :3]
         ;[(c :ii) :1 (c :iii) :1 (c :iv) :1 (conj (c :v) (note :C5)) :3]
         0.25 3 [:amp 0 :dur 1 :coef 0.01 :attack 1 :release 2 :vib 0]))
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

(defn rnd-chord
  ([] [7.75 0.25])
  ([b]
   (if (= b 1)
     (s/chord-p
      rise-pad
      (map midi->hz
           (chord-degree
            (choose [:i :v :vi :iii])
            :B3
            :major 4))
      [:coef 0.01 :amp 0.2 :dur 2 :attack 0.1 :release 4 :t 7]
      ))
   )
  )

(comment
  (s/add-p core/player scatterbrain :sc)
  (s/add-p core/player ambient :harmony)
  (s/add-p core/player melissa :harmony)
  (do
    (s/add-p core/player melissa-motif :motif)
    (s/add-p core/player techno.drums/melissa-b :main)
    )

  (let [[i iv v vi] (map #(map midi->hz
                               (chord-degree
                                %
                                :C4
                                (choose [:minor]) 4)) [:i :iv :v :vi])]
    (s/add-p core/player
             (s/fit-p {1.75 []}
              (s/phrase-p
               rise-pad
               [iv v vi i :15]
               0.25 15)) :motif)
    )


  (s/add-p
   core/player
   (s/phrase-p
    rise-fall-pad
    [(map midi->hz (chord :C3 :M7)) (map midi->hz (chord :G3 :M7)) :34]
    0.25 32 [:t 5])
   :harmony)

  (s/add-p
   core/player
   (fn [b]
     (if (or (integer? b) (weighted-coin 0.3))
       [piano [(choose (scale :C5 :major)) :dur 3]])
    )
   :motif)


  (let [mk-prog (fn [p]
                (conj
                 (vec
                  (mapcat #(vector (chord-degree % :C4 :major)) p)) :28))
      a (mk-prog [:ii :iii :i])
      b (mk-prog [:iv :iii :i])]
    (s/play-p
     (s/phrase-p
      piano
      a
      0.25 1 [:hard 0.3 :dur 2])))

  (s/play-p
   (s/phrase-p
    ks1
    [[:C4 :F4 :Ab3] :D4 [:Eb4 :G5]]
    0.25 3 [:decay 1 :amp 0.5 :cutoff-freq 2000]
    )
   1
   )
  (s/add-p
   core/player
   (s/fit-p {1.75 []}
            (s/phrase-p
             vintage-bass
             [:D3 :3 :C4 :1 :B4 :6]
             0.25 0 [:amp 1.5]))
   :bass)
(s/mod-p core/player :clap :use-counter true)
  (let [a [:D3 :F4]
        b [:C#3 :E4]
        c [:C3 :E4]
        e [:B3 :D4]]
    (s/play-p
     (s/phrase-p
      bass-synth
      [a a a a a a b b b b b c c c c c e e e e e]
      ;[c e]
      0.25 1 [:attack 0.1 :release 0.3])
     )
    )


  (s/set-arg core/player :click :amp 0.2)

  (s/add-p core/player coffee :harmony)
  (s/add-p core/player untitled :harmony)
  (s/add-p core/player untitled-f :motif)
  (s/add-p core/player rnd-chord :motif)


  (let []
    (s/add-p
     core/player
     (s/phrase-p
      plk-bass
      [:Eb3]
      0.25 3)
     :bass)
   )
  (s/rm-p core/player :bass)
  (def d (drone-noise (midi->hz (note :Ab2)) :amp 0.07))
  (ctl d :amp 0.1)
  (ctl d :freq (midi->hz (note :E)))
  (def w (wobble-drone (midi->hz (note :E2)) :amp 0.2))
  (ctl w :amp 0.05)
  (ctl w :freq (midi->hz (note :C4)))
  (ctl w :wobble 1)
  (kill d)
  (kill w)
  (s/play-p
   (s/chord-p
    bass-synth
    (chord :D3 :m7)))
  (kill 86334)

  (s/add-p
   core/player
   (fn [b]
     (if (or (= (rand-int 4) 0)
          (integer? b) (= (mod b (int b)) 0.5) (= (mod b (int b)) 0.75)
          )
       (let [notes (concat (chord-degree :ii :C4 :minor 4) (chord-degree :iii :C4 :minor 4)
                           (chord-degree :v :C4 :minor 4))]
         [(choose [piano]) [(choose notes) :dur 1 :amp 0.2 :coef 0.05]
          (choose [piano]) [(choose notes) :dur 1 :amp 0.15 :coef 0.05]]
         )))
   :motif2)

  (s/set-amp core/player :a 0.2)
  (s/set-arg core/player :main3 :amp 0.5)
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

  (s/add-p
   core/player
   (let [[a b] [[:G3 :D4] [:G3 :Eb4]]
         [c d] [[:A3 :E4] [:A3 :F4]]]
       (s/phrase-p
        ks1
        [a b a b :2 a b :2 a b a b a b :2
         c d c d :2 c d :2 c d c d c d :2]
        0.25 0 [:coef 0.01 :dur 2]))
   :motif2)
  (s/rm-p core/player :motif2)

  (bass-synth (midi->hz (note (choose (scale :F#2 :locrian)))) :release 3)

  (s/play-p
   ;core/player
   (let [root :C4
         n 4
         [a b c d e f g]
         (map #(chord-degree % root :minor n) [:i :ii :iii :iv :v :vi :vii])]
     (s/phrase-p
      reverb-test
                                        ;[f e d b]
      [e a d b]
      0.25 3
      [:decay 3 :delay-time 0.4 :release 1 :dur 2 :amp 0.3 :coef 0.01]
                                        ;      {:refresh 0.6 :sputter 0.5 :sputter-amt 0.25 :reverse 0}
      ))
   1.6
   3
   ;:motif
   )

  (s/play-p
                                        ;core/player
   (let [a (chord :C4 :M7)
         b (chord :B3 :M7)
         ]
     (s/phrase-p
      klang-test
                                        ;[a a a a a :0 b b b b b :1]
      [a a a a a
       :0 b b b b b :1
       ]
      0.25 2 [:dur 0.8 :amp 0.5 :vib 0]))
                                        ;:harmony
   1.6
   )

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
