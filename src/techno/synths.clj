(ns techno.synths
  (:use [overtone.core]
        )
  )


(overtone.helpers.lib/defrecord-ifn Sc-synth [name params]
  (fn [syn & args]
    (let [args (if args args [])
          target (if (vector? (first args)) (first args) nil)
          args (if (vector? (first args)) (rest args) args)]
        (node (:name syn)
              (into {} (map vec (partition 2 args)))
              (if target {:position (first target) :target (second target)})))))



(defsynth sweet [note 60 dur 1 amp 1 vib 0.02 out-bus 0]
  (let [freq (midicps note)
        ratios (map float [1 3/4 1/5 2/7 11/5 5/8])
        freqs (map #(* % freq) ratios)
        freqs (map #(vibrato:kr % 3 vib)
                   freqs)
        amps (map #(float (/ 1 %)) (range 1 (inc (count ratios))))
        sig (klang:ar [freqs amps])
        attack (* 0.2 dur)
        sustain (* 0.4 dur)
        release (* 0.4 dur)
        env (env-gen (envelope [0 0.6 0.4 0] [attack sustain release]) :action 2)
        sig (* sig env amp)]
    (out:ar out-bus [sig sig])
    )
  )

(defsynth piano [note 60 amp 1 dur 1 vel 100 decay 0.8 release 0.8 hard 0.8 velhard 0.8 muffle 0.8 velmuff 0.8 velcurve 0.8 stereo 0.2 tune 0.5 random 0.1 stretch 0.1 sustain 0.1 out-bus 0]
  (let [freq (midicps note)
        env (env-gen (perc (/ 1 vel) dur) :action 2)
        snd (* amp env (mda-piano freq 1 vel  decay  release  hard  velhard  muffle  velmuff  velcurve  stereo  tune  random  stretch  sustain))
        snd2 (comb-n snd 0.2 0.2 dur)]
    (out:ar out-bus [snd snd])
    )
  )


(defsynth bpfsaw [note 60 dur 1 atk 0.3 detune 0 rq 0.2 amp 1 pan 0 out-bus 0]
  (let [freq (midicps note)
        env (env-gen (perc (* atk dur) (* (- dur atk) dur)) :action 2)
        sig (sync-saw (+ freq (* detune freq)))
        sig (* env (bpf sig freq rq) amp)
        ;sig (balance2 sig sig pan)
        ]
    (out out-bus [sig sig])
    )
  )

(defsynth flute [note 60  amp 0.5  attack 0.1  decay 0.3  sustain 0.4  release 0.2 dur 3  trill 0 out-bus 0]
  (let [freq (midicps note)
        [a d s r] (map #(* dur %) [attack decay sustain release])
        env  (env-gen (adsr-ng a d s r) :action FREE)
        mod1 (lin-lin:kr (sin-osc:kr 6) -1 1 (* freq 0.99) (* freq 1.01))
        mod3 (lin-lin:kr (sin-osc:kr trill) -1 1 0.1 1)
        sig (distort (* env (sin-osc [freq mod1])))
        sig (* amp sig mod3)]
    (out out-bus sig)
    ))

(defsynth bpfsaw2 [freq 500 atk 2 sus 0 rel 3 c1 1 c2 -1
		 detune 0.2 pan 0 cfhzmin 0.1 cfhzmax 0.3
		cfmin 500 cfmax 2000 rqmin 0.1 rqmax 0.2
                   lsf 200 ldb 0 hsf 6000 hdb 0 amp 1 rs 0.5 out-bus 0]
  (let [env (env-gen:kr (envelope [0 1 1 0] [atk sus rel] [c1 0 c2]) :action 2)
        f (* freq (midiratio (* (lf-noise0:kr 0.5) detune)))
        sig (saw [f f])
        noise (lin-exp
               (lf-noise1:kr
                (lin-exp (lf-noise1:kr 4) -1 1 cfhzmin cfhzmax))
               -1 1 cfmin cfmax)
        sig (bpf sig noise (lin-exp (lf-noise1:kr 0.1) -1 1 rqmin rqmax))
        sig (b-low-shelf:ar sig lsf rs ldb)
        sig (b-hi-shelf:ar sig hsf rs hdb)
        sig (balance2 (first sig) (second sig))
        sig (* sig env amp)]
    (out out-bus sig)
    ))

(defsynth bass-synth [freq 200 attack 0.1 amp 1 release 1 detune 3 bwr 1 out-bus 0]
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
    (out:ar out-bus [sig sig])
    )
  )
;; (defn load-synth-descs []
;;   (let [syns (techno.sequencer/eval-sc "~descs = \"\";SynthDescLib.global.synthDescs.keys.do({|s| ~descs = ~descs + \"\\\"\" + s + \"\\\"\" + \",\"}); \"[\" +  ~descs + \"]\"" ":synths")
;;         a (read-string (first (deref syns 4000 [])))
;;         names (map #(do (.trim %)) (filter #(not (.contains % "system")) a))]
;;     (doseq [syn names]
;;       (let [p (techno.sequencer/eval-sc (str "SynthDescLib.at(\\" syn ").asString; ") ":desc")
;;             desc (first (deref p 3000 []))
;;             lines (clojure.string/split desc #"\n")
;;             params (reduce
;;                     (fn [params p]
;;                       (if (.startsWith p "ControlName")
;;                         (let [[_ _ _ name _ default] (clojure.string/split p #"\s+")]
;;                           (conj params {:name name :default (Float/parseFloat default)})
;;                           )
;;                         params)
;;                       ) [] lines)]
;;         (intern 'techno.synths (symbol syn) (map->Sc-synth {:name syn :params params :args (map #(get % :name) params)}))
;;         )
;;       )
;;     )
;;   )
;; (load-synth-descs)
