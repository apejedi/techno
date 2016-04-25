(ns techno.motifs
  (:use [overtone.core]
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
        attack (* 0.03 dur)
        sustain (* 0.5 dur)
        release (* 0.4 dur)
        env (env-gen (envelope [0 1 0.7 0] [attack sustain release]) :action 2)
        sig (* sig env amp)]
    (out:ar 0 sig)
    (out:ar 1 sig)
    )
  )

(def get-motif (atom (fn [_])))
(swap! get-motif (fn [_]
                   (fn [_]
                     (let [choices [[(note :Ab4)]
                                    [(note :G4)]
                                    [(note :C#4)]
                                    [(note :F5) (note :Ab5)]
                                    [(note :G5) (note :Bb5)]
                                    ]
                           oct 0
                           ;(* 12 (-> 2 rand int inc))
                           ]
                       (mapcat (fn [n] (vector sweet [(+ n oct) :amp 1 :dur 0.25]))
                               (choose choices))
                       ))))

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
                (fn [_]
                  (let [notes (choose [(chord :C4 :minor)
                                       (chord :D4 :major)
                                       (chord :Eb4 :dim)
                                       (chord :F4 :major)
                                       (chord :G3 :minor)])]
                    (mapcat #(vector sweet [% :amp 0.5 :dur 1])
                            notes)
                    ))))



(comment
  (sweet (choose (scale :Ab3 :major)))
  (s/play 1 @chords)
  (@get-motif)
  (def note-player (s/gets 2))
  (s/setsp note-player 2)
  (s/rmp note-player get-motif)
  (s/addp note-player chords)
  (kill note-player)
  (organ :gate (env-gen (perc 2)))
  (kill organ)
  )
