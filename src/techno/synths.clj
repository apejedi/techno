(ns techno.synths
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.sequencer :only [adsr-ng]])
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
                     (adsr-ng a d s r)
                     ;(adsr a d s r)
                     :gate gate
                     :action FREE)
              snd   (* env (apply + waves) amp)]
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

(defsynth flute [note 60  amp 0.5  attack 0.1  decay 0.3  sustain 0.4  release 0.2 dur 3  output 0]
  (let [freq (midicps note)
        [a d s r] (map #(* dur %) [attack decay sustain release])
        env  (env-gen (adsr-ng a d s r) :action FREE)
        mod1 (lin-lin:kr (sin-osc:kr 6) -1 1 (* freq 0.99) (* freq 1.01))
        mod2 (lin-lin:kr (lf-noise2:kr 1) -1 1 0.2 1)
        mod3 (lin-lin:kr (sin-osc:kr (ranged-rand 4 6)) -1 1 0.5 1)
        sig (distort (* env (sin-osc [freq mod1])))
        sig (* amp sig
               ;mod2 mod3
               )]
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

(defsynth zap [freq1 5000 freq2 100 dur 0.1 amp 0.2]
  (let [freq (x-line freq1 freq2 dur :action 2)
        sig (* (lf-tri freq) amp)]
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



(defsynth whistle [freq1 200 freq2 300 dur 5 freq1-sus 0.4 freq2-sus 0.4 mod 10 amp 1]
  (let [[a b c] [(* freq1-sus dur) (* (- 1 freq1-sus freq2-sus 0.1) dur) (* freq2-sus dur)]
        env (env-gen:kr
             (envelope [freq1 freq1 freq2 freq2 0]
                       [a b c 1] :exponential) :action FREE)
        osc-a (* amp 0.5 (sin-osc env))]
    (out:ar [0 1] osc-a)
    )
  )

(defsynth reverb-test [freq 440 max-delay 0.2 delay-time 0.2 decay 1]
  (let [osc (klang [(map #(* freq %) [1 2 6]) [0.6 0.2 0.2]])
        env (env-gen (perc))
        env2 (env-gen (perc 0.1 decay) :action FREE)
        snd (* env osc 0.5)
        snd2 (comb-n snd max-delay delay-time decay)
        snd2 (* snd2 0.4 env2)]
    (out [0 1] (+ snd snd2))
    )
  )

(defsynth plk-bass [note 42 out-bus 0 dur 1 amp 1]
  (let [freq (midicps note)
        subfreq (/ freq 2)
        subenv (env-gen (perc 0 dur) :action FREE)
        env (env-gen (perc 0 (/ dur 2)))
        plk (* (pluck (pink-noise) 1 0.2 (/ 1 subfreq)) subenv 2)
        tri (* (var-saw freq) env)
        sin (* (sin-osc freq) env)
        sub (tanh (* (sum (sin-osc [subfreq (- subfreq 2) (+ subfreq 2)])) subenv))
        click (* (sum (rlpf (impulse:ar 0) [2000 8000] 1)) 1000)
        sig (+ plk tri sub click)
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
        sig (* (moog-ff sig (x-line (* freq 150) (* freq 30) 0.1) 0.1) amp)]
    (out [0 1] sig)
   )
  )
(defsynth bing [note   72 attack 0.02 decay  0.3 amp 1]
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
        snd (delay-c snd 1.5 1)
        snd (* snd (env-gen:kr (adsr-ng (* 0.1 dur) (* 0.1 dur) (* 0.6 dur) (* 0.2 dur)) :action 2))
        snd (* amp (allpass-c snd 0.5 0.05 0.3))
        ]
    (out [0 1] snd)
    )
  )

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
