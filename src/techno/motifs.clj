(ns techno.motifs
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.sequencer :as s])
  )

(defsynth sweet [note 60 dur 1 amp 1]
  (let [freq (midicps note)
        ratios [1 3/4 1/5 2/7 11/5 5/8]
        freqs (map #(* % freq) ratios)
        freqs (map #(vibrato:kr % 3 0.02)
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
                 (let [n (choose (scale :C5 :minor))]
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
                     (s/adsr-ng a d s r)
                     ;(adsr a d s r)
                     :gate gate
                     :action FREE)
              snd   (* env (apply + waves) amp)]
          (out 0 snd)
          (out 1 snd)
          )
        )

(defonce chords (atom []))

(swap! chords (fn [_]
                (let [get-p (fn [d] (mapcat
                                    #(vector sweet [%
                                                       :amp 0.2
                                                       :dur 1
                                        ;:attack 0.01
                                        ;:release 0.7
                                                    ])
                                    (chord-degree d :C4 :minor 4)))]
                    {1 (get-p :vi)
                     2 (get-p :v)
                     3 (get-p :iv)
                     4 (get-p :ii)
                     4.75 []
                     })
                ))

(defonce arpeggio (atom nil))
(swap! arpeggio (fn [_]
                  (let [root :C5
                        type :minor
                        args [:coef 0.01 :amp 0.3 :dur 2]
                        inst ks1]
                    ;; (s/arp-p piano
                    ;;          (chord-degree
                    ;;           :v
                    ;;           root type 4)
                    ;;          args 0)
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



(comment
  (s/play 2 @arpeggio)
  (s/set-sp core/player (/ 80 60))
  (overpad2 (choose (scale :Ab3 :major)))
  (rise-fall-pad)
  (stop)

  (s/play 1 [(s/chord-p piano :C4 (choose [:M7 :m7 :11 :9sus4]) [:amp 0.1 :dur 2])])
  (s/add-p core/player motif :motif)
  (s/play (choose (range 1 5)) @chords)
  (s/set-sp note-player 2)
  (s/add-p core/player chords :harmony)
  (s/add-p core/player arpeggio :arp)
  (s/rm-p core/player :arp)
  (s/wrap-p core/player :harmony false)
  (organ :gate (env-gen (perc 2)))
  (kill organ))
