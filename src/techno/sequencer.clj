(ns techno.sequencer
  (:use [overtone.core]
        [overtone.inst.drum]
        )
  )


(defonce ^:private patterns (atom {}))
(defonce ^:private sequencer-handlers (atom {}))
(defonce ^:private trigger-sources (atom {}))
(defonce ^:private trigger-buses (atom {}))
(defonce t-source-g (group "trigger sources"))
(defonce t-synth-g (group "trigger generators" :after t-source-g))


(def test-pattern
  (atom {
         1 [dance-kick []]
         2 [bing []]
         3 [bing []]
         4 [noise-snare []]
         5 [bing []]
         6 [dance-kick []]
         7 [bing []]
         8 [bing []]
         9 [noise-snare []]
         10 [dance-kick []]
         })
  )


(defn get-rand-int [min max]
  (+ (rand-int (- max min)) min)
  )

(defn get-val-if-ref [x]
  (if (instance? clojure.lang.Atom x)
    @x
    x
    )
  )
(defsynth syncopation-synth [freq 1 uid 0]
    "A synth to randomly modulate the clock speed of a trigger synth"
    (let [trigger (dust:kr freq)]
      (send-trig:kr trigger uid)
    )
    )


(defn syncopate
  "Creates a syncopation synth which randomly changes clock speed of a trigger synth"
  ([trig-synth] (syncopate trig-synth 1 5 10))
  ([trig-synth freq from to]
   (let [uid (trig-id)
         synth (syncopation-synth freq uid)
         ]
     (println "adding handler on " uid)
     (on-trigger uid
                 (fn [_]
                   (ctl trig-synth :clock-speed (get-rand-int from to)))
                 ::syncopate)
     synth
     ))
  )


(defn pp-pattern [pattern]
  (println "{")
  (doseq [i (sort (keys pattern))]
    (print i " ")
    (doseq [[instrument args] (partition 2 (pattern i))]
      (print "[" (.name instrument) args "]"))
    (println)
    )
  (println "}")
  )




(defn player
  [cur-beat pattern]
  (let [beat-actions
        (cond
          (fn? pattern) (pattern cur-beat)
          (map? pattern) (pattern (int cur-beat))
          (sequential? pattern)
          (if (< cur-beat (count pattern)) (nth pattern (dec cur-beat))))]
    (dorun
     ;(println "playing " beat-actions " for beat " cur-beat)
     (for [[instrument args] (partition 2 beat-actions)]
       (do
         (apply instrument args)
         )
       ))
    )
  )

(defsynth trigger-source [out-bus 3 clock-speed 2]
  (out:kr out-bus (impulse:kr clock-speed))
  )

(defsynth trigger-synth [listen-bus 3
                         uid 0 pattern-size 4]
  (let [trigger (in:kr listen-bus)
        count (stepper:kr trigger :min 1 :max pattern-size)]
    (send-trig:kr trigger uid count)
    )
  )


(defn- update-pattern-size [sequencer]
  (if (node-active? sequencer)
    (ctl sequencer :pattern-size
         (reduce (fn [c p]
                   (let [val (get-val-if-ref p)]
                     (max c
                          (cond
                            (map? val) (apply max (keys val))
                            (sequential? val) (count val)
                            true 1))
                     ))
                 1 (get @patterns (to-sc-id sequencer)))))
  )


(defn addp [sequencer pattern]
  "(addp sequencer pattern) adds the given pattern to the patterns being played by the sequencer"
  (let [id (to-sc-id sequencer)
        cur-val (get @patterns id [])
        watcher-key (keyword (gensym "pattern"))]
    (swap! patterns (fn [p]
                      (assoc p id (conj cur-val pattern))
                      ))
    (update-pattern-size sequencer)
    (if (instance? clojure.lang.Atom pattern)
      (do
        (add-watch pattern watcher-key (fn [& args] (update-pattern-size sequencer)))
        )
      )
    )
  )



(defn rmp [sequencer pattern]
    "(rmp sequencer pattern) stops the given pattern from being played"
  (let [id (to-sc-id sequencer)
        cur-val (get @patterns id [])]
    (swap! patterns (fn [p]
                      (assoc p id
                             (remove #(= (get-val-if-ref pattern) (get-val-if-ref %)) cur-val))
                      ))
    (update-pattern-size sequencer)
    )
  )

(defmacro create-anon-synth [bus ugen]
  `(synth []
          (out:kr ~bus ~ugen)
          )
  )

(defn- build-sequencer [source-synth bus]
  (let [uid (trig-id)
        synth (trigger-synth [:tail t-synth-g] bus uid 10)
        key (keyword (gensym "sequencer"))]

    (swap! trigger-buses (fn [buses]
                           (assoc buses (to-sc-id synth) bus)
                           ))
    (swap! trigger-sources (fn [sources]
                             (assoc sources (to-sc-id synth) source-synth)
                             ))
    (on-trigger synth uid
                (fn [beat]
                  (doseq [p (get @patterns (to-sc-id synth))]
                    (player beat (get-val-if-ref p))))
                key)
    (swap! sequencer-handlers (fn [handlers]
                                (assoc handlers (to-sc-id synth) key)))
    synth
    ))

(defn gets
  "Get default sequencer which uses impulse as source"
  ([] (get-sequencer 2))
  ([clock-speed]
   (let [trigger-bus (control-bus)]
     (build-sequencer
      (trigger-source [:tail t-source-g] trigger-bus clock-speed)
      trigger-bus)))
  )

(defn gcs [t-ugen]
  "Get custom sequencer with given ugen used as trigger source"
  (let [trigger-bus (control-bus)]
    (build-sequencer
     ((create-anon-synth trigger-bus t-ugen) [:tail t-source-g])
     trigger-bus))
  )



(defn get-source [sequencer]
  (@trigger-sources (to-sc-id sequencer))
  )

(defn set-source [sequencer source]
  (let [id (to-sc-id sequencer)
        bus (@trigger-buses id)
        source-synth ((create-anon-synth bus source) [:tail t-source-g])]
    (kill (@trigger-sources id))
    (swap! trigger-sources (fn [sources]
                             (assoc sources id source-synth)
                             ))

    )
  )

(defn kill-sequencer [sequencer]
  (let [id (to-sc-id sequencer)]
    (swap! patterns (fn [p] (dissoc p id)))
    (remove-event-handler (get sequencer-handlers id))
    (kill (@trigger-sources id))
    (swap! trigger-sources (fn [sources]
                             (dissoc sources id)
                             ))
    (free-bus (@trigger-buses id))
    (swap! trigger-buses (fn [buses]
                           (dissoc buses id)
                           ))
    (kill sequencer)
    )
  )

(defn replp [sequencer replacement]
  (swap! patterns (fn [p]
                    (assoc p (to-sc-id sequencer) replacement)))
  )

(defn getp
  ([] patterns)
  ([sequencer]
   (get patterns (to-sc-id sequencer) []))
  )
(defunk-env adsr-ng
  "Create an non-gated attack decay sustain release envelope
  suitable for use as the envelope parameter of the
  env-gen ugen.

  attack       - the time it takes to go from 0 to the
                 attack level (this defaults to 1)

  attack-level - level of the amplitude at the attack,
                 immediately before decay stage starts


  decay        - the time it takes to go from the specified
                 amplitude level to sustain * level (also
                 defaulting to 1)

  sustain      - sustain duration


  release      - the time it takes to go from the sustain
                 amplitude to 0

  level        - the level of the amplitude after the attack,
                 and the value to multiply the sustain
                 fraction with to determine the sustain
                 amplitude

  curve        - the envelope curve

  bias         - a value to add with every value of the
                 envelope

  This envelope has multiple phases: attack, decay, sustain and
  release. Once the attack phase has started, after the specified attack
  time, the envelope value is the attack-level + bias. Next the decay
  phase kicks in. After the decay time, the amplitude is at level +
  bias. The amplitude then stays at this level for sustain seconds, and
  then enters the release phase.  After release time, the amplitude
  is 0 + bias."
  [attack 0.01 decay 0.3 sustain 1 release 1
              attack-level 1 level 1 curve :linear bias 0]
  (with-overloaded-ugens
    (envelope
     (map #(+ % bias) [0 attack-level level level 0])
     [attack decay sustain release]
     curve)))
