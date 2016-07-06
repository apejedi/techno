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
                           (= (rand-int 4) 1)
                           ;true
                        )
                     [
                      flute [n1 :amp 0.15 :dur 1]
                      piano [n2 :amp 0.4 :dur 1]
                      ;sweet [n2 :amp 0.2 :dur 1 :coef 0.001]
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
                        args [:coef 0.001 :amp 0.5 :atk 0.01 :dur 1]
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

(def melissa
  (let [l [:dur 3]
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
  )

(def melissa-motif
  (s/phrase-p
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
  )

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
  (if (or (and (integer? b) (odd? b)))
    (s/chord-p overpad
               ;(chord (choose [:B4 :F#5]) (choose [:m7 :M7 :m9 :m13]))
               (chord-degree
                (choose [:i :iv :v :vi])
                :C4
                (choose [:minor]))
                [:coef 0.01 :amp 0.2 :dur 2 :attack 1 :release 3]
                )))
(comment
  (doseq [n (rand-chord :B3 :M7 4 16)]
    (piano n :dur 1)
    )
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

  (s/add-p core/player acid :acid {:sc303 1001}
           )
  (s/play-p acid 2.5)
  (ctl t :gate 0)
  (ctl t :gate 1 :freq 100 :dec 4 :sus 1 :wave 0)


  (s/rm-p core/player :random)
  (s/add-p core/player chords :harmony)
  (s/add-p core/player arpeggio :arp)
  (s/rm-p core/player :harmony)
  (s/wrap-p core/player :harmony true)
  (s/add-p core/player ted-guitar :guitar)
  (s/add-p core/player x-naut :x-naut)
  (s/add-p core/player
           (let [l [:amp 0.5 :attack 1 :release 3]]
             (s/phrase-p
              overpad
              [[:F4 :A4 :C5 :E4] :14
               [:F4 :A4 :C5 :E4] :14
               [:C4 l :E4 l :G4 l :B4] :17]
              0.25
              0
              [:amp 0.5 :attack 1.3 :release 2.5]))
           :motif
           ;2
           )
  (s/rm-p core/player :motif2)
  (s/play-p chords
            arpeggio
            1.2)
  )

(def x-naut (atom nil))
(swap! x-naut
       (fn [_]
         (s/merge-p
          (s/phrase-p
           overpad
           [(chord :C4 :M7) :6
            (chord :F3 :9sus4) :6]
           0.25 0 [:amp 0.2 :attack 1 :release 3])
          )))


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

(def song-of-storms-h
     (let [d (chord :D4 :minor)
           e (chord :E4 :minor)
           f (chord :F4 :major)]
       (s/phrase-p
        piano
        [:B3 [:D4 :F4] [:D4 :F4] :1 :C4 [:E4 :G4] :1
         :D4 [:F4 :A4] [:F4 :A4] :1 :C4 [:E4 :G4] :1]
        ;; [d d d d e :2
        ;;  f f f f e :2]
        0.25
        0
        [:amp 0.5 :attack 0.5 :release 1])))
(def song-of-storms-m
     (let []
       (s/phrase-p
        piano
        [:D5 :F5 :D6 :4 :D5 :F5 :D6 :4
         :E6 :2 :F6 :E6 :F6 :E6 :C6 :A5 :4
         :A5 :2 :D5 :2 :F5 :G5 :A5 :4
         :A5 :2 :D5 :2 :F5 :G5 :E5 :4
         ]
        0.125
        1
        [:amp 0.5 :dur 3])))

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
    (into [[sc303 [:freq 65.406395 :env 5000.0 :sus 0.0 :ctf 1294.8276 :res 1.0 :dec 1.4827586 :wave 1.0 :vol 0.2]]]
          (mapcat f freqs))
    )
  )
