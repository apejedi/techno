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
