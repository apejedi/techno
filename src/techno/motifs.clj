(ns techno.motifs
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.sequencer :as s])
  )

(defsynth sweet [note 60 dur 1 amp 1 vib 0.02]
  (let [freq (midicps note)
        ratios [1 3/4 1/5 2/7 11/5 5/8]
        freqs (map #(* % freq) ratios)
        freqs (map #(vibrato:kr % 3 vib)
                   freqs)
        amps (map #(/ 1 %) (range 1 (inc (count ratios))))
        sig (sin-osc freqs amps)
        attack (* 0.2 dur)
        sustain (* 0.4 dur)
        release (* 0.4 dur)
        env (env-gen (envelope [0 0.6 0.4 0] [attack sustain release]) :action 2)
        sig (* sig env amp)]
    (out:ar 0 sig)
    (out:ar 1 sig)
    )
  )


(defsynth bpfsaw [note 60 dur 1 atk 0.3 detune 0 rq 0.2 amp 1 pan 0]
  (let [freq (midicps note)
        env (env-gen (perc (* atk dur) (* (- dur atk) dur)) :action 2)
        sig (sync-saw (+ freq (* detune freq)))
        sig (* env (bpf sig freq rq) amp)
        sig (balance2 sig sig pan)]
    (out 0 sig)
    (out 1 sig)
    )
  )


(defsynth piano [note 60 amp 1 dur 1 vel 100 decay 0.8 release 0.8 hard 0.8 velhard 0.8 muffle 0.8 velmuff 0.8 velcurve 0.8 stereo 0.2 tune 0.5 random 0.1 stretch 0.1 sustain 0.1]
  (let [freq (midicps note)
        env (env-gen (perc (/ 1 vel) dur) :action 2)]
    (out:ar [0 1] (* amp env (mda-piano freq 1 vel  decay  release  hard  velhard  muffle  velmuff  velcurve  stereo  tune  random  stretch  sustain)))
    )
  )

(definst overpad2
  [note 60 amp 0.7 attack 0.001 release 2]
  (let [freq  (midicps note)
        env   (env-gen (perc attack release) :action FREE)
        f-env (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)))))
        bfreq (/ freq 2)
        sig   (apply +
                     (concat (* 0.7 (sin-osc [bfreq (* 0.99 bfreq)]))
                             (lpf (saw [freq (* freq 1.01)]) f-env)))
        ;sig (allpass-n sig 0.1 0.25)
        audio (* amp env sig)]
    audio))

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


(swap! chords (fn [_]
                (let [get-p (fn [d]
                              (s/chord-p sweet
                                         (chord-degree d :C4 :minor 4)
                                         [:amp 0.2 :dur 1])
                              )]
                    {1 (get-p :vi)
                     2 (get-p :v)
                     3 (get-p :iv)
                     4 (get-p :ii)
                     4.75 []
                     })
                ))

(defonce arpeggio (atom nil))
(swap! arpeggio (fn [_]
                  (let [root :C4
                        type :minor
                        args [:coef 0.01 :amp 0.5 :atk 0.01 :dur 2]
                        inst ks1]
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
  (let [inst piano
        args [:coef 0.001 :dur 2 :atk 0.01 :amp 0.7]
        t [:dur 4]
        s [:space 1]
        main [:C3 :A4 :E5 t s
              :B3 :D4 :E5 t s
              :A3 :F4 :C5 t s
              :G3 :E4 :B4 t s]
        switch [:F#3 :Eb4 :A4 t s
                :E3 :E4 :G4 t s
                :E3 :Eb4 :A4 t s
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
      switch2
      )
     (double (/ 1 4))
     0
     args
     )
    ))

(def melissa
  (let [l [:dur 3]
        main [:Eb3 :Bb4 :Eb3 :Bb4 l [:space 1] :Eb3 :Bb4]]
      (s/phrase-p
       ks1
       (concat [:Ab2 l] main
               [:Ab2 l] main
               [:F#2 l] main
               [:F#2 l] main
               [:Bb4] [:C5 l])
       (double (/ 1 4))
       0
       [:dur 2 :atk 0.001 :coef 0.01]
       )
    )
  )

(def melissa-motif
  (s/phrase-p
   bpfsaw
   [:C6 :Bb5 :G5 [:space 2] :F5 :Eb5 :F5 [:space 2] :Ab5 [:space 2]
    :Bb5 :G5 :Bb5 [:space 2]
    :C6 :Bb5 :F5 :Bb5]
   (double (/ 1 4))
   1
   [:dur 2 :atk 0.001]
   )
  )

(def coffee (atom nil))
(swap! coffee
       (fn [_]
         (let [root :A4
               type :major
               a (chord-degree :ii root type 3)
               b (chord-degree :iii root type 3)
               c (chord-degree :iv root type 3)]
             (s/phrase-p
              piano
              [a b c a [:space 0] b [:space 0] c]
              (double (/ 1 4))
              1
              [:dur 2]
              ))
         ))

(comment
  (piano (note :C#3))
  (def f (simple-flute))
  (ctl f :freq (midi->hz (choose (scale :G4 :major))))
  (s/play 4 melissa)
  (s/pp-pattern scatterbrain)
  (s/set-sp core/player 1)
  (overpad2 (choose (scale :Ab3 :major)))
  (rise-fall-pad)
  (doseq [i (chord :F#4 :minor)]
    (piano i)
    )
  (doseq [i (chord :A4 :minor)]
    (piano i)
    )
  (piano (note :A4))

  (s/add-p core/player scatterbrain :sc)
  (s/add-p core/player melissa :harmony)
  (s/add-p core/player melissa-motif :motif)
  (s/add-p core/player motif :motif)
  (s/add-p core/player coffee :harmony)

  (s/add-p core/player (s/arp-p ks1
                                ;(map note [:D4 :F#4 :B4])
                                (map note [:D4 :F#4 :Ab4])
                                [:coef 0.01] 0) :chord)
  (s/rm-p core/player :motif)
  (s/set-sp note-player 2)
  (s/add-p core/player chords :harmony)
  (s/add-p core/player arpeggio :arp)
  (s/rm-p core/player :arp)
  (s/wrap-p core/player :harmony true)
  (organ :gate (env-gen (perc 2)))
  (kill organ)
  )
