(ns techno.synths
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.sequencer :only [adsr-ng]])

  (:require [techno.sequencer :as s]))
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
(defsynth harmonic
  [amp 0.5 freq 100]
  (let [partials 20
        z-init   0
        offset   (line:kr 0 -0.02 60)
        snd (loop [z z-init
                   i 0]
              (if (= partials i)
                z
                (let [f (clip:kr (mul-add
                                   (lf-noise1:kr [(+ 6 (rand 4))
                                                  (+ 6 (rand 4))])
                                   0.2 offset))
                      src  (f-sin-osc (* freq (inc i)))
                      newz (mul-add src f z)]
                  (recur newz (inc i)))))]
    (out 0 (pan2 (* amp snd)))))

(defsynth sc303 [out-bus 0 freq 440 wave 0 ctf 100 res 0.2 sus 0 dec 1.0 env 1000 gate 0 vol 0.2]
  (let [v (Math/pow 10 -9)
        vol-env (env-gen (envelope [v 1 1 v] [0.01 sus dec] :exponential) gate)
        fil-env (env-gen (envelope [v 1 v] [0.01 dec] :exponential) gate)
        waves [(saw (* freq vol-env)) (pulse (* freq vol-env) 0.5)]
        ;; sig (rlpf:ar
        ;;      (select:ar wave waves)
        ;;      (* env fil-env)
        ;;      res)
        sig (rlpf:ar
             (select:ar wave waves)
             (+ ctf (* env fil-env))
             res)
        sig (* sig vol)]
    (out:ar out-bus [sig sig])
    )
  )

(defsynth organ
        [note 60 dur 2 amp 1]
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
                     (adsr-ng a d s r)
                     ;(adsr a d s r)
                     :action FREE)
              snd   (* env (apply + waves) amp 0.1)]
          (out 0 snd)
          (out 1 snd)
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

(defsynth flute [note 60  amp 0.5  attack 0.1  decay 0.3  sustain 0.4  release 0.2 dur 3  output 0 trill 0]
  (let [freq (midicps note)
        [a d s r] (map #(* dur %) [attack decay sustain release])
        env  (env-gen (adsr-ng a d s r) :action FREE)
        mod1 (lin-lin:kr (sin-osc:kr 6) -1 1 (* freq 0.99) (* freq 1.01))
        mod3 (lin-lin:kr (sin-osc:kr trill) -1 1 0.1 1)
        sig (distort (* env (sin-osc [freq mod1])))
        sig (* amp sig mod3)]
    (out output sig)
    ))

(defsynth piano [note 60 amp 1 dur 1 vel 100 decay 0.8 release 0.8 hard 0.8 velhard 0.8 muffle 0.8 velmuff 0.8 velcurve 0.8 stereo 0.2 tune 0.5 random 0.1 stretch 0.1 sustain 0.1]
  (let [freq (midicps note)
        env (env-gen (perc (/ 1 vel) dur) :action 2)
        snd (* amp env (mda-piano freq 1 vel  decay  release  hard  velhard  muffle  velmuff  velcurve  stereo  tune  random  stretch  sustain))
        snd2 (comb-n snd 0.2 0.2 dur)]
    (out:ar [0 1] snd)
    )
  )

(defsynth dub-kick
  [freq 80 amp 1]
  (let [cutoff-env (perc 0.001 1 freq -20)
        amp-env (perc 0.001 1 1 -8)
        osc-env (perc 0.001 1 freq -8)
        noiz (lpf (white-noise) (+ (env-gen:kr cutoff-env) 20))
        snd  (lpf (sin-osc (+ (env-gen:kr osc-env) 20)) 200)
        mixed (* (+ noiz snd) (env-gen amp-env :action FREE) 2 amp)]
    (out:ar 0 mixed)
    (out:ar 1 mixed))
    )
(definst dance-kick
  [freq  50.24
   attack  0.0001
   decay   0.484
   fattack  0.0001
   fdecay  0.012
   mul 8
   amp  0.8]
  (let [freq-env (env-gen:kr (perc fattack fdecay))
        wave (sin-osc (+ freq (* mul freq freq-env)))
        env  (env-gen:kr (perc attack decay) :action FREE)
        src (* env wave)
        dist (clip2 (* 2 (tanh (* 3 (distort (* 1.5 src))))) 0.8)
        eq (b-peak-eq dist 37.67 1 10.4)]
    (* amp eq)))

(defsynth zap [freq1 5000 freq2 100 dur 0.2 amp 1]
  (let [freq (x-line freq1 freq2 dur)
        env (env-gen:kr (perc (* 0.1 dur) (* 0.9 dur) amp) :action 2)
        sig (* (lf-tri freq) env)]
    (out:ar 0 sig)
    (out:ar 1 sig)
    )
  )


(defsynth bpfsaw2 [freq 500 atk 2 sus 0 rel 3 c1 1 c2 -1
		 detune 0.2 pan 0 cfhzmin 0.1 cfhzmax 0.3
		cfmin 500 cfmax 2000 rqmin 0.1 rqmax 0.2
                   lsf 200 ldb 0 amp 1 output 0]
  (let [env (env-gen:kr (envelope [0 1 1 0] [atk sus rel] [c1 0 c2]) :action 2)
        f (* freq (midiratio (* (lf-noise0:kr 0.5) detune)))
        sig (saw [f f])
        noise (lin-exp
               (lf-noise1:kr
                (lin-exp (lf-noise1:kr 4) -1 1 cfhzmin cfhzmax))
               -1 1 cfmin cfmax)
        sig (bpf sig noise (lin-exp (lf-noise1:kr 0.1) -1 1 rqmin rqmax))
        sig (b-low-shelf sig lsf 0.5 ldb)
        sig (balance2 (first sig) (second sig))
        sig (* sig env amp)]
    (out output sig)
    ))

(defsynth klang-test [freq 440 amp 1 atk 0.1 dur 3]
  (let [partials (map double
                      ;[1]
                      [(/ 1 2) (/ 2 3) 1 (/ 4 3) 2 (/ 5 2)]
                      )
        num (count partials)
        sig (klang [(map
                     #(* freq %)
                     ;#(lin-exp (lf-noise1:kr 0.001) -1 1 (* freq %) (* freq % 2))
                     partials)
                    [0.2 0.1 0.4 0.1 0.1 0.1]
                    (repeat num (double (/ 1 num)))
                    ])
        env (env-gen (perc (* atk dur) (* (- 1 atk) dur)) :action FREE)
        sig (* sig env amp)]
    (out 0 [sig sig])
    )
  )

(defsynth sin-inst [note 60 dur 2 amp 1]
  (let [env (env-gen (envelope [0.1 1 0] [(* 0.01 dur) (* 1 dur)] :welch) :action 2)]
    (out:ar [0 1] (* env
                     (+
                      (* (sin-osc (midicps note)))
                      (*  (sin-osc (midicps (+ 19 note))) 0.08)
                      (* (sin-osc (midicps (- note 12))) 0.04)
                      )
                      amp)))
  )

;; (defsynth kick [amp 1]
;;   (let [body-freq (env-gen:ar (envelope [261 120 51] [0.035 0.08]))
;;         body-amp (env-gen:ar (linen :attack-time 0.005 :release-time 0.4) :action 2)
;;         body (* body-amp (sin-osc:ar body-freq))
;;         ;; pop-freq (x-line:kr 750 261 0.02)
;;         ;; pop-amp (* (env-gen:ar (linen :attack-time 0.001 :release-time 0.021)) 0.15)
;;         ;; pop (* (sin-osc pop-freq) pop-freq)
;;         ;; click-amp (* (env-gen:ar (perc 0.001 0.01)) 0.15)
;;         ;; click (* (lpf:ar (formant:ar 910 4760 2110) 3140) click-amp)
;;         ;; snd (+ body pop click)
;;         ;snd (tanh (+ body pop click))
;;         ]
;;     (out:ar [0 1] (* snd amp))
;;     )
;;   )



(defsynth kick [freq      80
                amp       0.8
                mod-freq  5
                mod-index 5
                sustain   0.4
                noise     0.025 :min 0.001 :max 1.0 :step 0.001]
  (let [pitch-contour (line:kr (* 2 freq) freq 0.02)
        drum (lpf (sin-osc pitch-contour (sin-osc mod-freq (/ mod-index 1.3))) 1000)
        drum-env (env-gen (perc 0.005 sustain) :action FREE)
        hit (hpf (* noise (white-noise)) 500)
        hit (lpf hit (line 6000 500 0.03))
        hit-env (env-gen (perc))
        snd (* amp (+ (* drum drum-env) (* hit hit-env)))]
    (out:ar [0 1] snd))
  )

(definst snare [freq  405 amp  0.3
   sustain 0.1
   decay  0.1
   drum-amp 0.25
   crackle-amp 40
   tightness 1000]
  (let [drum-env  (env-gen (perc 0.005 sustain) :action FREE)
        drum-osc  (mix (* drum-env (sin-osc [freq (* freq 0.53)])))
        drum-s3   (* drum-env (pm-osc (saw (* freq 0.85)) 184 (/ 0.5 1.3)))
        drum      (* drum-amp (+ drum-s3 drum-osc))
        noise     (* 0.1 (lf-noise0 20000))
        noise-env (env-gen (perc 0.005 sustain) :action FREE)
        filtered  (* 0.5 (brf noise 8000 0.1))
        filtered  (* 0.5 (brf filtered 5000 0.1))
        filtered  (* 0.5 (brf filtered 3600 0.1))
        filtered  (* (brf filtered 2000 0.0001) noise-env)
        resonance (* (resonz filtered tightness) crackle-amp)]
    (* amp (+ drum resonance))))



(defsynth whistle [freq1 200 freq2 300 dur 5 freq1-sus 0.4 freq2-sus 0.4 mod 10 amp 1]
  (let [[a b c] [(* freq1-sus dur) (* (- 1 freq1-sus freq2-sus 0.1) dur) (* freq2-sus dur)]
        env (env-gen:kr
             (envelope [freq1 freq1 freq2 freq2 0.01]
                       [a b c 0.01] :exponential)
             :action FREE)
        osc-a (* amp 0.5 (sin-osc env))
        ;osc-a (hpf (lpf osc-a freq2) freq1)
        ;ticks (impulse:ar 20)
        ]
    (out:ar [0 1] osc-a)
    )
  )

(defsynth reverb-test [freq 440 max-delay 0.2 delay-time 0.2 decay 1 amp 1]
  (let [osc (klang [(map #(* freq %) [1 2 6]) [0.6 0.2 0.2]])
        env (env-gen (perc))
        env2 (env-gen (perc 0.1 decay) :action FREE)
        snd (* env osc 0.5)
        snd2 (comb-n snd max-delay delay-time decay)
        snd2 (* snd2 0.4 env2)]
    (out [0 1] (* (+ snd snd2) amp))
    )
  )

(defsynth plk-bass [note 42 out-bus 0 dur 0.5 amp 1 plk 2]
  (let [freq (midicps note)
        subfreq (/ freq 2)
        subenv (env-gen (perc 0 (* dur plk)) :action FREE)
        env (env-gen (perc 0 dur ))
        plk (* (pluck (pink-noise) 1 0.2 (/ 1 subfreq)) subenv 2)
        tri (* (var-saw freq) env)
        sin (* (sin-osc freq) env)
        sub (tanh (* (sum (sin-osc [subfreq (- subfreq 2) (+ subfreq 2)])) subenv))
        click (* (sum (rlpf (impulse:ar 0) [2000 8000] 1)) 1000)
        sig (+ plk tri sub)
        sig (rlpf sig (x-line (* freq 100) (* freq 10) 0.15))
        sig (+ sig (* (moog-ff sig (* freq 20) 2.5) 0.1))
        sig (b-peak-eq sig 400 0.5 -9)
        sig (b-peak-eq sig 2000 0.5 6)
        sig (b-hi-shelf sig 8000 1 3)
        sig (b-peak-eq sig 200 1 3)
        sig (* sig (x-line 1 0.6 0.1))
        sig (tanh sig)
        sig (+ sig (rlpf sig (x-line (* freq 100) (* freq 10) 0.15)) sin sub)
        sig (tanh (/ sig 2.3))
        sig (* (moog-ff sig (x-line (* freq 150) (* freq 30) 0.1) 0.1) amp)
        ]
    (out [0 1] sig)
   )
  )

(defsynth wire-bass [decay 0.5 coef 0.54 amp 1 dur 3]
  (let [coef (lin-lin coef 0 1 -0.3 1)
        decay (lin-lin decay 0 1 0.007 0.02)
        sig (pluck:ar (* (pink-noise) 0.1) 1 decay decay dur coef)
        sig (* sig (env-gen:kr (perc (* 0.001 dur) (* 0.999 dur)) :action FREE) amp)]
    (out:ar [0 1] sig)
    )
  )




(defsynth bass-synth [freq 200 attack 0.1 amp 1 release 1 detune 3 bwr 1]
  (let [freq-v (+
                (lin-exp (lf-noise0:kr 2) -1 1 0.1 detune)
                  freq)
        sig (var-saw [freq-v freq-v] 0 1)
        sig2 (* 0.02 (saw [freq freq]))
        sig (resonz sig freq bwr)
        env (env-gen (perc attack release) :action FREE)
        sig (+ sig sig2)
        sig (* sig amp env)
        ]
    (out:ar 0 sig)
    )
  )


(defsynth bing [note 72 attack 0.02 decay  0.3 amp 1]
  (let [snd (sin-osc (midicps note))
        env (env-gen (perc attack decay) :action FREE)]
    (out [0 1] (* 0.8 env snd amp))))


(defsynth chicago-pad [freq 440 cutoff 500 amp 1 dur 10]
  (let [freq (+ freq (sin-osc:kr 0.1) 20)
        freqs (map #(* freq %) [(/ 3 2) (/ 6 5) 1])
        snd (mix (* 0.3 (saw freqs)))
        snd (rlpf snd (+ 5000 (* 100 (sin-osc:kr 0.1))) 0.1)
        snd (* 0.2 (g-verb snd 40 10 0.6 0.6 -3 -9 -11))
        snd (moog-ff snd (+ cutoff (* (sin-osc:kr 0.08) (/ cutoff 10))) 3 0)
        ;; snd (delay-c snd 1.5 1)
        snd (* snd (env-gen:kr (adsr (* 0.1 dur) (* 0.1 dur) (* 0.6 dur) (* 0.2 dur)) :action 2))
        snd (* amp (allpass-c snd 0.5 0.05 0.3))
        ]
    (out [0 1] snd)
    )
  )


(defsynth clang [freq 100 amp 1 attack 0.1 decay 1 reps 10]
  (let [sig (ringz (impulse:ar reps) [200 400 234 889] 0.7)
        sig (sin (sum sig))
        sig (g-verb sig 5 2 0.7)
        env (env-gen:kr (perc attack decay) :action FREE)
        sig (* sig env)
        ]
    (out:ar [0 1] sig)
    )
  )

;; (defsynth chicago-pad2 [freq 440 cutoff 500 amp 1]
;;   (let [freq2 (/ (* (/ 3 2) freq) 2)
;;         freq3 (/ (* (/ 5 6) freq) 2)
;;         delay 0.05
;;         freq (+ freq (* (sin-osc:kr (/ freq 2)) (/ freq 2)))
;;         snd (* 0.1 (saw [freq (+ freq 1) (- freq 1) freq2 freq3]))
;;         snd (+ snd (* 0.1 (var-saw [freq (+ freq 1) (- freq 1) freq2
;;                                     freq3 (+ freq2 1) (+ freq3 1)]
;;                                    0 (range-lin:kr (lf-tri:kr 7.13) 0 0.1))))
;;         snd (mix snd)
;;         snd (* 16 (free-verb snd 0.51 10 0.1))
;;         snd (rlpf snd [(/ (+ 4000 (* (sin-osc:kr 0.1) 1700)) 20.51)
;;                        (/ (+ 700 (* (sin-osc:kr 4.2) 480)) 20)]
;;                   (sin-osc 0.1 (* 3.14 1.5)))]
;;     )
;;   (out:ar [0 1] snd)
;;   )


(defsynth drone-noise [freq 440 amp 1]
  (let [freqs (map #(* freq %) [2 4 1])
        sig (klank [freqs (repeat (count freqs) (/ 1 (count freqs)))] (pink-noise))
        sig (bpf (tanh sig) freq)
        sig (* sig 0.5 amp)
        ]
    (out:ar [0 1] sig)
    )
  )


(defsynth wobble-drone [freq 100 wobble 2 amp 1]
  (let [mod-f (/ freq 2)
        idx (* 10 (sin-osc wobble))
        sig (pm-osc freq mod-f idx)
                                        ;sig2 (var-saw freq :width (lin-lin (lf-noise0 3) -1 1 0 1))
        sig2 (lf-tri freq)
        sig (+ (* amp sig ) (* 0.4 sig2))]
    (out:ar [0 1] sig)
    )
  )
;; (def w (wobble-drone :amp 0.4 :wobble 10))
;; (ctl w :wobble 0.6)
;; (ctl w :amp 0.3)
;; (kill wobble-drone)

;; (defsynth sistres [note 60 dur 6 amp 1]
;;   (let [freq (midicps note)
;;         freqs (repeatedly 4
;;                 #(exp-rand (- freq (/ freq 128)) (+ freq (/ freq 128))))
;;         sig (splay:ar (klang:ar
;;                        [freqs (repeat (count freqs) (double (/ 1 (count freqs))))]))
;;         sig (* sig (lf-gauss dur 0.25 0 0 2) amp)
;;         ]
;;     (out:ar 0 sig)
;;     )
;;   )


(defsynth rise-pad [freq 440 t 3 attack 0.5 amp 1 detune 0.1]
  (let [dur (- 1 attack)
        [a d s r] [(* attack t) 0 (* 0.3 dur t) (* 0.7 dur t)]
        env (env-gen (adsr-ng :attack a :sustain s :decay d :release r) :action 2)
        freq (* freq (midiratio (* (lf-noise0:kr 0.5) detune)))
        sig  (blip freq 3)
        sig (* (bpf sig freq 0.5) 0.4)
        sig (* sig env amp)]
    (out:ar [0 1] sig)
    )
  )

(defsynth prophet
  "The Prophet Speaks (page 2)

   Dark and swirly, this synth uses Pulse Width Modulation (PWM) to
   create a timbre which continually moves around. This effect is
   created using the pulse ugen which produces a variable width square
   wave. We then control the width of the pulses using a variety of LFOs
   - sin-osc and lf-tri in this case. We use a number of these LFO
   modulated pulse ugens with varying LFO type and rate (and phase in
   some cases to provide the LFO with a different starting point. We
   then mix all these pulses together to create a thick sound and then
   feed it through a resonant low pass filter (rlpf).

   For extra bass, one of the pulses is an octave lower (half the
   frequency) and its LFO has a little bit of randomisation thrown into
   its frequency component for that extra bit of variety."

  [amp 1 freq 440 cutoff 1500 rq 0.3  attack 1 decay 2 out-bus 0 ]

  (let [snd (pan2 (mix [(pulse freq (* 0.1 (/ (+ 1.2 (sin-osc:kr 1)) )))
                        (pulse freq (* 0.8 (/ (+ 1.2 (sin-osc:kr 0.3) 0.7) 2)))
                        (pulse freq (* 0.8 (/ (+ 1.2 (lf-tri:kr 0.4 )) 2)))
                        (pulse freq (* 0.8 (/ (+ 1.2 (lf-tri:kr 0.4 0.19)) 2)))
                        (* 0.5 (pulse (/ freq 2) (* 0.8 (/ (+ 1.2 (lf-tri:kr (+ 2 (lf-noise2:kr 0.2))))
                                                           2))))]))
        snd (normalizer snd)
        env (env-gen (perc attack decay) :action FREE)
        snd (rlpf (* env snd snd) cutoff rq)]

    (out out-bus (* amp snd))))

;; (defsynth voice [freq 220 type 0 vib 0 amp 1 lg 0.5 depth 4 atk 0.1 dur 2]
;;   (let [data [[[400 750 2400 2600 2900]  [1 0.28 0.08 0.1 0.01] [0.1 0.1 0.04 0.04 0.04]]
;;               [[800 1150 2900 3900 4950] (map dbamp [0 -6 -32 -20 -50]) (map dbamp [80 90 120 130 140])] ;sopranoA 1
;;               [[350 2000 2800 3600 4950] (map dbamp [0 -20 -15 -40 -56]) (map dbamp [60 100 120 150 200])] ;sopranoE 2
;;               [[270 2140 2950 3900 4950] (map dbamp [0 -12 -26 -26 -44]) (map dbamp [60 90 100 120 120])] ;sopranoI 3
;;               [[450 800 2830 3800 4950] (map dbamp [0 -11 -22 -22 -50]) (map dbamp [70 80 100 130 135])] ;sopranoO 4
;;               [[325 700 2700 3800 4950] (map dbamp [0 -16 -35 -40 -60]) (map dbamp [50 60 170 180 200])] ;sopranoU 5
;;               [[800 1150 2800 3500 4950] (map dbamp [0 -4 -20 -36 -60]) (map dbamp [80 90 120 130 140])] ;altoA 6
;;               [[400 1600 2700 3300 4950] (map dbamp [0 -24 -30 -35 -60]) (map dbamp [60 80 120 150 200])] ;altoE 7
;;               [[350 1700 2700 3700 4950] (map dbamp [0 -20 -30 -36 -60]) (map dbamp [50 100 120 150 200])] ;altoI 8
;;               [[450 800 2830 3500 4950] (map dbamp [0 -9 -16 -28 -55]) (map dbamp [70 80 100 130 135])] ;altoO 9
;;               [[325 700 2530 3500 4950] (map dbamp [0 -12 -30 -40 -64]) (map dbamp [50 60 170 180 200])] ;altoU 10
;;               [[660 1120 2750 3000 3350] (map dbamp [0 -6 -23 -24 -38]) (map dbamp [80 90 120 130 140])] ;counterTenorA 11
;;               [[440 1800 2700 3000 3300] (map dbamp [0 -14 -18 -20 -20]) (map dbamp [70 80 100 120 120])] ;counterTenorE 12
;;               [[270 1850 2900 3350 3590] (map dbamp [0 -24 -24 -36 -36]) (map dbamp [40 90 100 120 120])] ;counterTenorI 13
;;               [[430 820 2700 3000 3300] (map dbamp [0 -10 -26 -22 -34]) (map dbamp [40 80 100 120 120])] ;counterTenorO 14
;;               [[370 630 2750 3000 3400] (map dbamp [0 -20 -23 -30 -34]) (map dbamp [40 60 100 120 120])] ;counterTenorU 15
;;               [[650 1080 2650 2900 3250] (map dbamp [0 -6 -7 -8 -22]) (map dbamp [80 90 120 130 140])] ;tenorA 16
;;               [[400 1700 2600 3200 3580] (map dbamp [0 -14 -12 -14 -20]) (map dbamp [70 80 100 120 120])] ;tenorE 17
;;               [[290 1870 2800 3250 3540] (map dbamp [0 -15 -18 -20 -30]) (map dbamp [40 90 100 120 120])] ;tenorI 18
;;               [[400 800 2600 2800 3000] (map dbamp [0 -10 -12 -12 -26]) (map dbamp [40 80 100 120 120])] ;tenorO 19
;;               [[350 600 2700 2900 3300] (map dbamp [0 -20 -17 -14 -26]) (map dbamp [40 60 100 120 120])] ;tenorU 20
;;               [[600 1040 2250 2450 2750] (map dbamp [0 -7 -9 -9 -20]) (map dbamp [60 70 110 120 130])] ;bassA 21
;;               [[400 1620 2400 2800 3100] (map dbamp [0 -12 -9 -12 -18]) (map dbamp [40 80 100 120 120])] ;bassE 22
;;               [[250 1750 2600 3050 3340] (map dbamp [0 -30 -16 -22 -28]) (map dbamp [60 90 100 120 120])] ;bassI 23
;;               [[400 750 2400 2600 2900] (map dbamp [0 -11 -21 -20 -40])  (map dbamp [40 80 100 120 120])] ;bassO 24
;;               [[350 600 2400 2675 2950] (map dbamp [0 -20 -32 -28 -36]) (map dbamp [40 80 100 120 120])]] ;bassU 25
;;         freqs (map first data)
;;         amps (map second data)
;;         qs (map #(nth % 2) data)
;;         in (saw (lag:kr
;;                  freq
;;                  ;(vibrato:kr freq depth vib)
;;                  0.2))
;;         env (env-gen (perc (* atk dur) (* (- 1 atk) dur)) :action 2)
;;         snd (* (b-band-pass in
;;                             (select:kr type freqs)
;;                             ;; (lag:kr
;;                             ;;  (select:kr type freqs)
;;                             ;;     lg)
;;                             (map dbamp [0 -11 -21 -20 -40])
;;                             ;(select:kr type amps)
;;                             ;; (lag:kr
;;                             ;;  (select:kr type amps)
;;                             ;;  lg)
;;                             )
;;                ;(select:kr type qs)
;;                (map dbamp [0 -11 -21 -20 -40])
;;                ;; (lag:kr
;;                ;;  (select:kr type qs)
;;                ;;  lg)
;;                )
;;         sig (mix:ar [snd snd])
;;         sig (* sig env amp)]
;;     (poll:kr (impulse:kr 1) qs "qs: ")
;;     (out 0 sig)
;;     (out 1 sig)
;;     )
;;   )


(defsynth horn [freq 440]
  (let [sin (klang [[freq (/ freq 2) (* freq (/ 3 5))] [0.6 0.3 0.1]] )
        s-aw (* (saw freq) 0.1)
        p (* (pulse freq (lin-lin (lf-noise0:kr 10) -1 1 0.5 1)) 0.1)
        sig (mix [sin s-aw])
        sig (free-verb sig 0.3 0.7 0.3)
        ]
    (out:ar [0 1] sig)
    )
  )


(defsynth bass2 [atk 0.001 decay 0.6 amp 1 freq 80 cutoff 2000 cutoff2 2000]
  (let [sig (* (decay2:ar (impulse:ar (/ atk 2)) atk decay)
               (mix (pulse:ar [freq (+ freq 1)] 0.3)) amp)
        sig (moog-ff sig (x-line:kr cutoff cutoff2 atk) 3)
        sig (* sig (env-gen (perc atk decay) :action 2))]
    (out:ar [0 1] sig)
    )
  )


(def dull-partials
  [
   0.56
   0.92
   1.19
   1.71
   2
   2.74
   3
   3.76
   4.07])
(def partials
  [
   0.5
   1
   3
   4.2
   5.4
   6.8])
(defcgen bell-partials
  "Bell partial generator"
  [freq {:default 440 :doc "The fundamental frequency for the partials"}
   dur  {:default 1.0 :doc "Duration multiplier. Length of longest partial will
                            be dur seconds"}
   partials {:default [0.5 1 2 4] :doc "sequence of frequencies which are
                                        multiples of freq"}]
  "Generates a series of progressively shorter and quieter enveloped sine waves
  for each of the partials specified. The length of the envolope is proportional
  to dur and the fundamental frequency is specified with freq."
  (:ar
   (apply +
          (map
           (fn [partial proportion]
             (let [env      (env-gen (perc 0.01 (* dur proportion)))
                   vol      (/ proportion 2)
                   overtone (* partial freq)]
               (* env vol (sin-osc overtone))))
           partials ;; current partial
           (iterate #(/ % 2) 1.0)  ;; proportions (1.0  0.5 0.25)  etc
           ))))


(definst dull-bell [freq 220 dur 1.0 amp 1.0]
  (let [snd (* amp (bell-partials freq dur dull-partials))]
    (detect-silence snd :action FREE)
    snd))

(definst pretty-bell [note 60 dur 1.0 mul 1.0]
  (let [
        freq (midicps note)
        snd (* mul (bell-partials freq dur partials) (env-gen (perc 0.01 1) :action FREE))
        ]
    ;(detect-silence snd :action FREE)
    snd))

(defsynth risset [pan 0 freq 400 amp 0.1 dur 2 atk 0.01]
  (let [amps [1 0.67 1 1.8 2.67 1.67 1.46 1.33 1.33 1 1.33]
        durs [1 0.9 0.65 0.55 0.325 0.35 0.25 0.2 0.15 0.1 0.075]
        frqs [0.56 0.56 0.92 0.92 1.19 1.7 2 2.74 3 3.76 4.07]
        dets [0 1 0 1.7 0 0 0 0 0 0 0]
        src (mix (map (fn [a du f de]
                        (let [env (env-gen (perc 0.005 (* dur du) a -4.5))]
                          (* amp env (sin-osc:ar (* freq (+ de f))))))
                      amps durs frqs dets))
        src (* src (env-gen (perc (* atk dur) (* (- 1 atk) dur)) :action FREE))]
    (out:ar [0 1] (pan2 src pan))
    )
  )
