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
                 (let [n (choose (scale :C4 :minor))]
                   (if (or (= (- b (int b)) 0.5) (= (rand-int 2) 1))
                     [overpad [n :amp 0.2]
                      piano [n :amp 0.1 :dur 3]
                      ])
                   )
                 )
               ;; (let [notes (scale :C4 :minor)
               ;;       inst piano
               ;;       dur 1
               ;;       amp 0.4]
               ;;   {
               ;;    1.5 [inst [(nth notes 7) :amp amp :dur dur]]
               ;;    2.5 [inst [(nth notes 4) :amp amp :dur dur]]
               ;;    3.5 [inst [(nth notes 2) :amp amp :dur dur]]
               ;;    4.5 [inst [(nth notes 3) :amp amp :dur dur]]
               ;;    4.75 []
               ;;    })
               ))


(defsynth organ
        [note 60 dur 2 amp 0.1 gate 1]
        (let [freq  (midicps note)
              [a d s r] (map * (repeat 4 dur) [0.01 0.2 0.5 0.2])
              waves (sin-osc [(* 0.5 freq)
                              freq
                              (* (/ 3 2) freq)
                              (* 2 freq)
                              (* freq 2 (/ 3 2))
                              (* freq 2 2)
                              (* freq 2 2 (/ 5 4))
                              (* freq 2 2 (/ 3 2))
                              (* freq 2 2 2)])
              env   (env-gen
                     ;(s/adsr-ng a d s r)
                     (adsr a d s r)
                     :gate gate
                     :action FREE)
              snd   (* env (apply + waves) amp)]
          (out 0 snd)
          (out 1 snd)
          )
        )

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
                                  [:amp 0.2 :dur 1 :coef 0.01])
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
                        args [:coef 0.01 :amp 0.3 :atk 0.01 :dur 2]
                        inst bpfsaw]
                    (concat (s/arp-p inst
                                     (chord-degree
                                      :v
                                      root type 4)
                                     args 0 8)
                            (s/arp-p inst
                                     (chord-degree
                                      :iv
                                      root type 4)
                                     args 0 8)
                            )
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
         (let [root :A4
               type :major
               a [:A4 :A5 :A6 :C#4 :F#5]
               b [:B4 :B5 :F#4]
               c [:C#4 :C#5 :C#6 :F#4]
               | [:space 0]]
             (s/phrase-p
              piano
              [a b c]
              (double (/ 1 4))
              1
              [:dur 2 :amp 0.7]
              ))
         ))

(comment
  (s/add-p core/player scatterbrain :sc)
  (s/add-p core/player ambient :background)
  (s/add-p core/player melissa :harmony)
  (do
    (s/add-p core/player melissa-motif :motif)
    (s/add-p core/player techno.drums/melissa-b :main)
    )
  (s/add-p core/player motif :motif)
  (s/add-p core/player coffee :harmony)
  (s/add-p core/player untitled :harmony)
  (s/add-p core/player untitled-f :motif)
  (s/add-p core/player (fn [b]
                         (if (or (integer? b) true)
                           (s/chord-p ks1 (chord-degree
                                           (choose [:i :iii :iv :v :vi])
                                           :C4 :minor)
                                      [:coef 0.01 :amp 0.3]))
                         ) :motif)

  (s/rm-p core/player :motif)
  (s/add-p core/player chords :harmony)
  (s/add-p core/player arpeggio :arp)
  (s/rm-p core/player :harmony)
  (s/wrap-p core/player :harmony true)
  (s/add-p core/player ted-guitar :guitar)
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
    {1 (s/chord-p inst (chord :G4 :m7) args)
     2 (s/chord-p inst (chord :F#3 :M7) args)
     2.5 []
     }
    )
  )
