(ns techno.sequencer
  (:use [overtone.core]
        [overtone.sc.machinery.server.comms]
        [overtone.inst.drum]
        )
  )


(defonce ^:private patterns (atom {}))
(defonce ^:private sequencer-handlers (atom {}))
(defonce ^:private trigger-sources (atom {}))
(defonce ^:private trigger-buses (atom {}))
(defonce ^:private sequence-buffers (atom {}))
(defonce t-source-g (group "trigger sources"))
(defonce t-synth-g (group "trigger generators" :after t-source-g))

(declare sync-s)

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

(defn- to-str [inst]
  (if (and (map? inst) (contains? inst :name))
    (:name inst)
    inst
    )
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
  (when (map? pattern)
    (println "{")
    (doseq [i (sort (keys pattern))]
      (print i " ")
      (doseq [[instrument args] (partition 2 (pattern i))]
        (print "[" (to-str instrument) (to-str args) "]"))
      (println)
      )
    (println "}"))
  (when (sequential? pattern)
    (println "[")
    (doseq [i pattern]
      (print " [")
      (doseq [[instrument args] (partition 2 i)]
        (print (to-str instrument) (to-str args)))
      (println "]")
      )
    (println "]")
    )
  )




(defn play
  "Function to play instruments on the given beat"
  ([cur-beat pattern] (play cur-beat pattern cur-beat))
  ([cur-beat pattern orig-beat]
   (let [beat-actions
         (cond
           (fn? pattern) (pattern cur-beat)
           (map? pattern) (pattern cur-beat)
           (and (sequential? pattern))
           (if (<= orig-beat (count pattern)) (nth pattern (dec orig-beat))))]
     ;; (println cur-beat)
     ;; (if (> (count beat-actions) 0)
     ;;   (println "playing "
     ;;            (reduce (fn [a b] (str (to-str a) " " (to-str b) " ")) beat-actions)
     ;;            " for beat " cur-beat " raw-beat" orig-beat))
     (dorun
      (for [[instrument args] (partition 2 beat-actions)]
        (if (not (nil? instrument))
          (let [inst (apply instrument args)]
            (when (and (instance? overtone.studio.inst.Inst instrument) ;If instrument has a gate argument, set it to 0 after trigger
                       (some #(= (:name %) "gate") (:params instrument)))
              (at (+ (now)
                     (* (if (>= (.indexOf args :dur) 0)
                          (nth args (inc (.indexOf args :dur)))
                          1
                          ) 1000))
                  (ctl inst :gate 0)
                  )
              )
            ))))))
  )

(defn- handle-beat-trigger [synth beat step]
  (let [
        step-beat (fn [beat step]
                    (let [res (inc (* (dec beat) step))]
                      (cond (=  (mod res (int res)) 0.0)
                           (int res)
                           true res)))
        stepped-beat (step-beat beat step)]
    ;(println "orig " beat " stepped beat " stepped-beat " time " (java.util.Date.))
    (doseq [[k p] (get @patterns (to-sc-id synth))]
      (let [val (get-val-if-ref (p :data))
            wrap (and (p :wrap) (not (fn? val)))
            size (cond
                   (map? val) (apply max (keys val))
                   (sequential? val) (count val)
                   true 1)
            wrap-beat (fn [beat size step]
                        ;(println "beat " beat " size " size " step " step)
                        (let [s (+ 1 (/ (- size 1) step))
                              steps (cycle (range 1 (inc s)))
                              w (nth steps (dec beat))]
                          (step-beat w step)
                          )
                        )
            final-beat (if (and wrap (> stepped-beat size))
                         (wrap-beat beat size step)
                         stepped-beat)
            orig-beat (if (and wrap (> beat size))
                         (wrap-beat beat size 1)
                         beat)]
        ;(println "playing " k " with beat " final-beat " orig " orig-beat " time " (.getTime (java.util.Date.)))
        (play final-beat val orig-beat)
        ))
    )
  )

(defsynth trigger-source [out-bus 3 clock-speed 2 step 0.25]
  (out:kr out-bus (impulse:kr
                   (/ clock-speed step)
                   ))
  )

(defsynth trigger-synth [listen-bus 3 uid 0 pattern-size 4 step 1 reset 0]
  (let [trigger (in:kr listen-bus)
        count   (stepper:kr trigger :min 1 :max (+ 1 (/ (- pattern-size 1) step))
                                 :reset reset :resetval 0)]
    (send-trig:kr trigger uid count)
    )
  )


(defn- update-pattern-size [sequencer]
  (if (node-active? sequencer)
    (ctl sequencer :pattern-size
         (reduce (fn [c p]
                   (let [val (get-val-if-ref (p :data))
                         step (node-get-control sequencer :step)
                         step (if (not (nil? step)) step 0.25)]
                     (max c
                          (cond
                            (map? val)  (apply max (keys val))
                            (sequential? val) (inc (* (dec (count val))
                                                      step))
                            true (if (node-get-control sequencer :pattern-size)
                                   (node-get-control sequencer :pattern-size)
                                   1)
                            ))
                     ))
                 1
                 (vals (get @patterns (to-sc-id sequencer)))
                 )))
  )


(defn add-p
  "(add-p sequencer pattern) adds the given pattern to the patterns being played by the sequencer"
  ([sequencer pattern] (add-p sequencer pattern (gensym "pat") {:wrap true}))
  ([sequencer pattern key] (add-p sequencer pattern key {:wrap true}))
  ([sequencer pattern key attrs]
   (when (node-active? sequencer)
     (let [id (to-sc-id sequencer)
           cur-val (get @patterns id {})
           is-atom (instance? clojure.lang.Atom pattern)
           watcher-key (if is-atom (keyword (gensym "pattern")))
           attrs (if is-atom (merge attrs {:watcher watcher-key}) attrs)]
       (swap! patterns (fn [p]
                         (if (and (contains? cur-val key) (contains? (get cur-val key) :watcher))
                           (remove-watch (get-in cur-val [key :data]) (get-in cur-val [key :watcher])))
                         (assoc p id
                                (assoc cur-val key (merge {:data pattern} attrs))
                                        ;(conj cur-val pattern)
                                )
                         ))
       (update-pattern-size sequencer)
       (if (= (count (keys cur-val)) 0)
         (sync-s sequencer)
         )
       (if is-atom
         (do
           (add-watch pattern watcher-key (fn [& args] (update-pattern-size sequencer)))
           )
         )
       )))
  )



(defn rm-p [sequencer pattern]
    "(rm-p sequencer pattern) stops the given pattern from being played"
  (let [id (to-sc-id sequencer)
        cur-val (get @patterns id {})
        val (if (keyword? pattern)
              (get cur-val pattern)
              (first (filter #(= (get-val-if-ref pattern) (get-val-if-ref (% :data)))
                             (vals cur-val))))]
    (if  (and (not (nil? val)) (contains? val :watcher))
      (remove-watch (get val :data) (get val :watcher)))
    (swap! patterns (fn [p]
                      (if (= pattern :all)
                        (assoc p id {})
                        (assoc p id
                               (if (keyword? pattern)
                                 (dissoc cur-val pattern)
                                 (into {}
                                       (remove
                                        (fn [[k v]]
                                          (= (get-val-if-ref pattern) (get-val-if-ref (v :data)))
                                          )
                                        cur-val
                                        )))
                               ))
                      ))
    (update-pattern-size sequencer)
    )
  )

(defmacro create-anon-synth [bus ugen & args]
  (if (= (type ugen) overtone.sc.machinery.ugen.sc_ugen.SCUGen)
    `(synth []
            (out:kr ~bus ~ugen)
            )
    `(synth []
            (out:kr ~bus (~ugen ~@args))
            )
    )
  )

(defn- cleanup [in]
  (let [id (to-sc-id (in :node))
        trigger-source (@trigger-sources id)
        trigger-bus (@trigger-buses id)
        sequence-handler (@sequencer-handlers id)
        sequence-buffer (@sequence-buffers id)
        do-if (fn [in action] (if (not (nil? in)) (apply action [in])))]
    (swap! patterns (fn [p] (dissoc p id)))

    (do-if sequence-handler remove-event-handler)

    (if (node-active? trigger-source)
      (do-if trigger-source kill)
      )
    (swap! trigger-sources (fn [sources]
                             (dissoc sources id)
                             ))
    (do-if trigger-bus free-bus)
    (swap! trigger-buses (fn [buses]
                           (dissoc buses id)
                           ))
    (do-if sequence-buffer buffer-free)
    (swap! sequence-buffers (fn [b]
                              (dissoc b id)))
    )
  )

(defn- build-sequencer
  ([source-synth bus] (build-sequencer source-synth bus 1))
  ([source-synth bus step]
   (let [uid (trig-id)
         synth (trigger-synth [:tail t-synth-g] bus uid 4 step)
         key (keyword (gensym "sequencer"))]
     (swap! trigger-buses (fn [buses]
                            (assoc buses (to-sc-id synth) bus)
                            ))
     (swap! trigger-sources (fn [sources]
                              (assoc sources (to-sc-id synth) source-synth)
                              ))
     (on-trigger synth uid
                 (fn [beat]
                   (handle-beat-trigger synth beat (node-get-control source-synth :step))
                   ) key)
     (swap! sequencer-handlers (fn [handlers]
                                 (assoc handlers (to-sc-id synth) key)))
     (on-node-destroyed synth cleanup)
     synth
     )))

(defn get-s
  "Get default sequencer which uses impulse as source"
  ([] (get-s 2 1))
  ([speed] (get-s speed 1))
  ([clock-speed step]
   (let [trigger-bus (control-bus)]
     (build-sequencer
      (trigger-source [:tail t-source-g] trigger-bus clock-speed step)
      trigger-bus
      step)))
  )

(defn g-cs [t-ugen]
  "Get custom sequencer with given ugen used as trigger source"
  (let [trigger-bus (control-bus)]
    (build-sequencer
     ((create-anon-synth trigger-bus t-ugen) [:tail t-source-g])
     trigger-bus))
  )


(defn g-bs
  "Sequencer runs using a buffer created using offsets"
  ([offsets] (g-bs offsets 0))
  ([offsets gap]
   (let [trigger-bus (control-bus)
         start (first offsets)
         end (last offsets)
         dur (- end start)
         sample-rate (/ 44.1 64) ;;sample rate in milliseconds
         size (+ (Math/ceil (* dur sample-rate)) (* gap sample-rate))
         buf (buffer size 1)
         digits (-> (str size) count dec)
         indices (map #(Math/round (* (- % start) sample-rate))
                      offsets)
         trigger (build-sequencer
                  ((create-anon-synth trigger-bus play-buf:kr 1 buf :loop 1) [:tail t-source-g])
                  trigger-bus)]
     (doseq [offset indices]
       (buffer-set! buf offset 0.9))
     (swap! sequence-buffers (fn [buffers] (assoc buffers (to-sc-id trigger) buf)))
     trigger
     ))
  )


(defn sync-s [& sequencers]
  "Synchronize given sequencers, meaning all will be reset to start their cycles at the same time"
  (let [ids (map to-sc-id sequencers)
        sizes (map #(node-get-control % :pattern-size) ids)
        size (apply max sizes)]
    (if size
      (ctl ids :pattern-size size))
    (ctl ids :reset 0)
    (ctl ids :reset 1)
    )
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



(defn repl-p [sequencer replacement]
  ([sequencer replacement]
   (repl-p sequencer replacement nil)
   )
  ([sequencer replacement key]
   (swap! patterns (fn [p]
                     (assoc p (to-sc-id sequencer) replacement))))
  )

(defn get-p
  ([] patterns)
  ([sequencer]
   (keys (get patterns (to-sc-id sequencer) {})))
  )

(defn set-sp [sequencer speed]
  (ctl (get-source sequencer) :clock-speed speed)
  )

(defn- mod-p [sequencer pattern attr val]
  (swap! patterns (fn [p]
                     (let [id (to-sc-id sequencer)
                           key (if (keyword? pattern) pattern
                                   (first (first
                                           (filter (fn [[k v]]
                                                     (= (v :data)  pattern)) (p id)))))]
                       (if (not (nil? key))
                         (assoc-in p [id key attr] val)
                         p
                         )
                       ))))

(defn wrap-p
  ([sequencer pattern] (wrap-p sequencer pattern true))
  ([sequencer pattern val]
   (mod-p sequencer pattern :wrap val)
   )
  )


(defn chord-p [in notes & [args]]
  "returns a beat action to play a chord using the given instrument
e.g. (chord-p inst (chord :C4 :minor)) -> [inst [note1] inst [note2] inst [note3]]"
  (mapcat #(vector in (cons % (if (nil? args) [] args)))
          notes)
  )

(defn build-rest-p [pattern]
  (vec (reduce (fn [res cur]
                 (if (= (first cur) :space)
                   (concat res (vec (repeat (second cur) nil)))
                   (conj (vec res) cur))
                 )
               []
               pattern))
  )
(defn arp-p [in notes & [args space reps]]
  "build arpeggio (arp-p bass [60 62 66] [:amp 0.5] & space)"
  (build-rest-p
       (apply concat
              (map #(vector [in (cons % (if (nil? args) [] args))]
                            [:space (if (nil? space) 1 space)]
                            )
                   (apply concat (repeat (if (nil? reps) 1 reps) notes))
                   )
              )
       )
  )

(defn phrase-p [inst phrase step & [space args]]
  (loop [phrase phrase beat 1 pattern {}]
    (let [args (if (not (nil? args)) args [])
          get-note #(if (number? %) % (note %))
          mk-action (fn [action block]
                      (reduce
                       (fn [a c]
                         (if (sequential? c)
                           (conj (vec (butlast a)) (into (last a) c))
                           (conj a inst (conj args :note (get-note c)))))
                       action
                       (vec block)))
          cur (first phrase)
          is-note (or (keyword? cur) (number? cur))
          is-space? #(and (sequential? %) (= (first %) :space))
          is-arg? #(and (sequential? %) (not (is-space? %)) (keyword? (first %)) (number? (second %)))
          is-arg (is-arg? cur)
          is-space (is-space? cur)
          is-block (and (not is-note) (not is-space) (not is-arg))
          action (get pattern beat [])
          action (cond
                   is-note (conj action inst (conj args :note (get-note cur)))
                   is-arg (conj (vec (butlast action)) (into (last action) cur))
                   is-block (mk-action action cur)
                   true nil)
          pattern (if (and (not (nil? action)) (> (count action) 0))
                    (assoc pattern beat action) pattern)
          space  (cond is-space (second cur)
                       (not (nil? space)) space
                       true 0)
          pattern (if (and (= (count (rest phrase)) 0) (> space 0))
                    (assoc pattern (+ beat (* space step)) [])
                    pattern)
          beat (if (or (is-arg? (second phrase)) (is-space? (second phrase)))
                 beat
                 (+ beat (* (inc space) step)))
          beat (if (= (mod beat (int beat)) 0.0) (int beat) beat)]
      ;(println "action " action " beat " beat " phrase " (rest phrase) " space " space)
      (if (> (count (rest phrase)) 0)
        (recur (rest phrase) beat pattern)
        pattern
        )
      )
    )
  )


(defn set-size [sequencer size]
  (ctl sequencer :pattern-size size)
  )
(defn set-st [sequencer step]
  (when (node-active? sequencer)
    (ctl (@trigger-sources (to-sc-id sequencer)) :step step)
    (ctl sequencer :step step)
    )
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
