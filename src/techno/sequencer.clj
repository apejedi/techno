(ns techno.sequencer
  (:use [overtone.core]
        [overtone.sc.machinery.server.comms]
        [overtone.inst.drum]
        )
  (:import (java.util.concurrent.atomic AtomicInteger))
  )


(defonce ^:private patterns (atom {}))
(defonce ^:private sequencer-handlers (atom {}))
(defonce ^:private trigger-sources (atom {}))
(defonce ^:private trigger-buses (atom {}))
(defonce ^:private sequence-buffers (atom {}))
(defonce ^:private pattern-counters (atom {}))
(defonce ^:private sequencer-data (atom {}))
(defonce t-source-g (group "trigger sources"))
(defonce t-synth-g (group "trigger generators" :after t-source-g))

(declare sync-s)


(defn sputter
  "Returns a list where some elements may have been repeated.

   Repetition is based on probabilty (defaulting to 0.25), therefore,
   for each element in the original list, there's a chance that it will
   be repeated. (The repetitions themselves are also subject to further
   repetiton). The size of the resulting list can be constrained to max
   elements (defaulting to 100).

  (sputter [1 2 3 4])        ;=> [1 1 2 3 3 4]
  (sputter [1 2 3 4] 0.7 5)  ;=> [1 1 1 2 3]
  (sputter [1 2 3 4] 0.8 10) ;=> [1 2 2 2 2 2 2 2 3 3]
  (sputter [1 2 3 4] 1 10)   ;=> [1 1 1 1 1 1 1 1 1 1]
  "
  ([list]          (sputter list 0.25))
  ([list prob]     (sputter list prob 100))
  ([list prob max] (sputter list prob max []))
  ([[head & tail] prob max result]
    (if (and head (< (count result) max))
      (if (< (rand) prob)
        (recur (cons head tail) prob max (conj result head))
        (recur tail prob max (conj result head)))
      result)))


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

(defn p-size [val & [step]]
  "Returns size of a pattern"
  (let [val (get-val-if-ref val)
        step (if step step 0.25)]
    (cond
      (fn? val) (if (= 0 (-> val class .getDeclaredMethods first .getParameterTypes alength))
                  (first (val))
                  1)
      (map? val)  (apply max (keys val))
      (sequential? val) (inc (* (dec (count val)) step))
      true 0
      )
    ))

(defn get-step [pattern]
  (let [val (get-val-if-ref pattern)
        sizes (cond (map? val) (keys val)
                    (and (fn? val)
                         (= 0 (-> val class .getDeclaredMethods first .getParameterTypes alength)))
                    (range 1 (first (val)) (second (val)))
                    true [1])]
    (reduce (fn [s b]
              (min s
                   (loop [s 1.0 b b]
                     (if (some #{(double (mod b (int b)))} (range 0.0 1 s))
                       s
                       (recur (/ s 2) b)))
                   ))
            1
            sizes)
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

(defn set-st [sequencer step]
  (when (node-active? sequencer)
    (ctl (@trigger-sources (to-sc-id sequencer)) :step step)
    (ctl sequencer :step step)
    ;(update-pattern-size sequencer)
    )
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
   (let [data (get-val-if-ref (pattern :data))
         beat-actions
         (cond
           (fn? data) (data cur-beat)
           (map? data) (data cur-beat)
           (and (sequential? data))
           (if (<= orig-beat (count data)) (nth data (dec orig-beat))))]
     ;; (println cur-beat)
     ;; (if (> (count beat-actions) 0)
     ;;   (println "playing "
     ;;            (reduce (fn [a b] (str (to-str a) " " (to-str b) " ")) beat-actions)
     ;;            " for beat " cur-beat " raw-beat" orig-beat))
     (loop [actions (partition 2 beat-actions) ret false]
       (let [[instrument args] (first actions)
             p (if (not (nil? instrument))
                 (if (and (or (= (type instrument) overtone.studio.inst.Inst)
                              (= (type instrument) overtone.sc.synth.Synth))
                          (some #(= (:name %) "gate") (:params instrument)))
                   (let [name (keyword (:name instrument))
                         inst-node (get pattern name)]
                     (if inst-node
                       (do
                         (apply ctl (concat [inst-node] (vec args)))
                         false)
                       (let [n (apply instrument args)]
                         (assoc pattern name n)
                         ))
                     )
                   (do (apply instrument args)
                       false))
                 )
             ret (if (map? p) p ret)]
         (if (> (count actions) 1)
           (recur (rest actions) ret)
           ret)
         )
       )
     ))
  )

(defn- handle-beat-trigger [synth beat step one-shot]
  (let [step-beat (fn [beat step]
                    (let [res (inc (* (dec beat) step))]
                      (cond (=  (mod res (int res)) 0.0)
                            (int res)
                           true res)))
        stepped-beat (step-beat beat step)
        id (to-sc-id synth)
        seq-size (get-in @sequencer-data [id :size])]
    ;; (println "orig " beat " stepped beat " stepped-beat " seq-size " seq-size
    ;;          ;" time " (java.util.Date.)
    ;;          )
    (if (number? seq-size)
      (doseq [[k p] (get @patterns id)]
        (let [val (get-val-if-ref (p :data))
              size (p-size val step)
              raw-size (if (sequential? val) size (+ 1 (/ (- size 1) step)))
              min-wrap (get p :min-wrap 0)
              wrap (and (get p :wrap true)
                        (>= (- seq-size stepped-beat) min-wrap)
                        (if (or (not (fn? val)) (and (fn? val) (> size 1))) true false))
              wrap-beat (fn [beat size step]
                          (let [steps (cycle (range 1 (inc raw-size)))
                                w (nth steps (dec beat))]
                            (step-beat w step)))
              counter (get @pattern-counters k)
              ;; meh (.compareAndSet counter 0 (int beat))
              ;; final-beat (if wrap
              ;;              (step-beat (.get counter) step)
              ;;              stepped-beat)
              ;; orig-beat (if wrap
              ;;             (.get counter)
              ;;             beat)
              ;; next-beat (if (>= (.get counter) raw-size)
              ;;             (.getAndSet counter 1)
              ;;             (.incrementAndGet counter))
              final-beat (if (get p :use-counter false)
                           (let [meh (.compareAndSet counter 0 (int beat))]
                             (if wrap
                               (step-beat (.get counter) step)
                               stepped-beat))
                           (if (and wrap (> stepped-beat size))
                             (wrap-beat beat size step)
                             stepped-beat))
              orig-beat (if (get p :use-counter false)
                          (let [orig-beat (if wrap
                                            (.get counter)
                                            beat)
                                next-beat (if (>= (.get counter) raw-size)
                                            (.getAndSet counter 1)
                                            (.incrementAndGet counter))]
                            orig-beat)
                          (if (and wrap (> beat size))
                            (wrap-beat beat size 1)
                            beat))
              new-p (play final-beat p orig-beat)]
          (if (map? new-p)
            (swap! patterns
                   (fn [cur]
                     (assoc-in cur [id k] new-p)
                     )))
          ;; (if (or (= k :a)
          ;;          false)
          ;;   (println "playing " k " with beat " final-beat " orig " orig-beat
          ;;            ;(.get counter) " size " size
          ;;                                 ;" time " (.getTime (java.util.Date.))
          ;;            ))
          )))
    (if (number? seq-size)
      (if (and one-shot (>= stepped-beat seq-size))
        (kill synth)
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
        count   (stepper:kr trigger :min 1
                            :max (+ 1 (/ (- pattern-size 1) step))
                            :reset reset :resetval 0)]
    (send-trig:kr trigger uid count)
    )
  )



(defn- update-pattern-size [sequencer]
  (if (node-active? sequencer)
    (let [step (node-get-control sequencer :step)
          step (if step step 0.25)
          seq-patterns (vals (get @patterns (to-sc-id sequencer)))
          get-size (fn [p]
                     (let [size-p (p-size (p :data) step)]
                       (cond
                         (contains? p :size) (p :size)
                         (> size-p 0) size-p
                         true (if (number?
                                   (node-get-control sequencer :pattern-size))
                                (node-get-control sequencer :pattern-size)
                                1)
                         )
                       ))
          size (reduce (fn [c p]
                         (let [val (get-val-if-ref (p :data))
                               cur (get-size p)]
                           (if (or (nil? c) (nil? cur))
                             (println c cur val p))
                            (max c cur)
                           ))
                       1
                       seq-patterns)
          data (get @sequencer-data (to-sc-id sequencer) {})
          p-step (if (> (count seq-patterns) 0)
                     (apply min
                            (map #(get-step (:data %))
                                 seq-patterns)))]
      (swap! sequencer-data
             (fn [seq-data]
               (assoc seq-data (to-sc-id sequencer) (assoc data :size size))
               ))
      (if (and (not (= step p-step)) (not (nil? p-step)))
        (set-st sequencer p-step))
      (ctl sequencer :pattern-size size)
      )
    )
  )

(defn rm-p [sequencer pattern]
    "(rm-p sequencer pattern) stops the given pattern from being played"
  (let [id (to-sc-id sequencer)
        cur-val (get @patterns id {})
        val (if (or (keyword? pattern) (symbol? pattern))
              (get cur-val pattern)
              (first (filter #(= (get-val-if-ref pattern) (get-val-if-ref (% :data)))
                             (vals cur-val))))]
    (if  (and (not (nil? val)) (contains? val :watcher))
      (remove-watch (get val :data) (get val :watcher)))
    (doseq [[k v] val]
      (if (and (= (type v) overtone.sc.node.SynthNode) (node-active? v))
        (kill v)
        )
      )
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
    (swap! pattern-counters
           (fn [c]
             (dissoc c pattern)
             ))
    (update-pattern-size sequencer)
    )
  )

(defn add-p
  "(add-p sequencer pattern) adds the given pattern to the patterns being played by the sequencer"
  ([sequencer pattern] (add-p sequencer pattern (gensym "pat") {:wrap true}))
  ([sequencer pattern key] (add-p sequencer pattern key {:wrap true}))
  ([sequencer pattern key attrs]
   (when (and (node-active? sequencer) (not (nil? pattern)))
     (let [id (to-sc-id sequencer)
           cur-val (get @patterns id {})
           is-atom (instance? clojure.lang.Atom pattern)
           watcher-key (if is-atom (keyword (gensym "pattern")))
           attrs (if is-atom (merge attrs {:watcher watcher-key}) attrs)]
       (if (contains? cur-val key)
         (rm-p sequencer key))
       (swap! patterns (fn [p]
                         (assoc p id
                                (assoc cur-val key (merge {:data pattern} attrs))
                                )
                         ))
       (swap! pattern-counters
              (fn [c]
                (assoc c key (AtomicInteger. 0))
                ))
       (update-pattern-size sequencer)
       (if is-atom
         (do
           (add-watch pattern watcher-key (fn [& args] (update-pattern-size sequencer)))
           )
         )
       )))
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

(defn- cleanup
  ([] (doseq [id (keys @patterns)]
        (cleanup {:node id})))
  ([in]
   (let [id (to-sc-id (in :node))
         trigger-source (@trigger-sources id)
         trigger-bus (@trigger-buses id)
         sequence-handler (@sequencer-handlers id)
         sequence-buffer (@sequence-buffers id)
         do-if (fn [in action] (if (not (nil? in)) (apply action [in])))]
     (doseq [k (keys (@patterns id))] (rm-p (in :node) k))
     (swap! patterns (fn [p] (dissoc p id)))

     (do-if sequence-handler remove-event-handler)
     (swap! sequencer-handlers (fn [handlers]
                                 (dissoc handlers id)
                                 ))
     (swap! sequencer-data (fn [data] (dissoc data id)))

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
     ))
  )

(defn- build-sequencer
  ([source-synth bus] (build-sequencer source-synth bus 1 false))
  ([source-synth bus step one-shot]
   (let [uid (trig-id)
         listen-to (if one-shot 3 bus)
         synth (trigger-synth [:tail t-synth-g] listen-to uid 4 step)
         key (keyword (gensym "sequencer"))]
     (swap! trigger-buses (fn [buses]
                            (assoc buses (to-sc-id synth) bus)
                            ))
     (swap! trigger-sources (fn [sources]
                              (assoc sources (to-sc-id synth) source-synth)
                              ))
     (on-trigger synth uid
                 (fn [beat]
                   (handle-beat-trigger synth beat (node-get-control source-synth :step) one-shot)
                   ) key)
     (swap! sequencer-handlers (fn [handlers]
                                 (assoc handlers (to-sc-id synth) key)))
     (on-node-destroyed synth cleanup)
     synth
     )))

(defn get-s
  "Get default sequencer which uses impulse as source"
  ([] (get-s 2 0.25 false))
  ([speed] (get-s speed 0.25 false))
  ([speed step] (get-s speed step false))
  ([clock-speed step one-shot]
   (let [trigger-bus (control-bus)]
     (build-sequencer
      (trigger-source [:tail t-source-g] trigger-bus clock-speed step)
      trigger-bus
      step
      one-shot)))
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

(defn mod-p [sequencer pattern attr val]
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

  (let [note-arg (if (or (instance? overtone.studio.inst.Inst in)
                         (instance? overtone.sc.synth.Synth in))
                     (if (some #(= (:name %) "freq") (:params in))
                       :freq
                       :note)
                     :note)
        get-note #(if (number? %) %
                      (if (= note-arg :freq)
                        (midi->hz (note %))
                        (note %)))
        notes (map get-note notes)
        args (if (nil? args) [] args)]
    (mapcat #(vector in (cons note-arg (cons % args)))
            notes)
    )
  )

(defn build-rest-p [pattern & [step]]
  (let [to-int #(if (= (mod % (int %)) 0.0)
                  (int %) %)
        pat (reduce (fn [res cur]
                  (let [step (if step step 0.25)
                        key (apply max (keys res))
                        key (to-int key)
                        ;; prev (get res key)
                        ;; key (if (= prev []) (- key step) key)
                        is-space (or (and (sequential? cur) (= (first cur) :space))
                                     (and (keyword? cur) (re-find #"^\d" (name cur))))
                        next-key (if is-space
                                   (+ key (* step (if (sequential? cur) (second cur)
                                                      (-> cur name Integer/parseInt))))
                                   (+ step key))
                        next-key (to-int next-key)
                        res (assoc res next-key (if (and is-space (> next-key key)) [] nil))]
                    (if is-space
                      res
                      (assoc res key cur))
                    ))
                {1 nil}
                pattern)
        tail (apply max (keys pat))
        prev (get pat tail)
        pat (if (= prev []) (assoc (dissoc pat tail)
                                   (to-int (- tail (if step step 0.25))) [])
                pat)]
    (if (nil? (get pat tail))
      (dissoc pat tail)
      pat)
    )
  ;; (vec (reduce (fn [res cur]
  ;;                (if (or (and (sequential? cur) (= (first cur) :space)) (keyword? cur))
  ;;                  (concat res (vec (repeat
  ;;                                    (cond (sequential? cur) (second cur)
  ;;                                          true (-> cur name Integer/parseInt))
  ;;                                           nil)))
  ;;                  (conj (vec res) cur))
  ;;                )
  ;;              []
  ;;              pattern))
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

(defn merge-p [& patterns]
  (reduce
   (fn [res cur]
     (reduce
      (fn [p b]
        (let [val (get p b)
              to-add (get cur b)
              new-val (if (sequential? val)
                        (concat val to-add)
                        to-add)]
          (assoc p b new-val)))
      res
      (keys cur))
     )
   {}
   patterns)
  )

(defn stretch-p [pattern & [new-size]]
  (let [step (reduce (fn [s b]
                       (min s (if (= (double (mod b (int b))) 0.0)
                                b
                                (mod b (int b))))) 1 (keys pattern))
        tail (apply max (keys pattern))
        quantize #(+ 1 (/ (- % 1) step))
        new-map (zipmap (map quantize (keys pattern)) (vals pattern))
        size (quantize tail)
        new-size (quantize (if new-size new-size (* tail 2)))
        beats (cycle (range 1 (inc size)))]
    (reduce
     (fn [p b]
       (let [cur-beat (+ 1 (* (- b 1) step))
             wrapped (double (nth beats (dec b)))
             action (get new-map wrapped)]
         (if (or action (= b new-size))
           (assoc p cur-beat action)
           p))
       ) pattern (range (inc size) (inc new-size)))
    )
  )

(defn m-phrase [m-args base step]
  (let [to-int #(if (= (mod % (int %)) 0.0)
                  (int %) %)
        size (p-size base)
        mem (atom {:def (map to-int (range 1 size step))
                   :cur (map to-int (range 1 size step))})
        refresh (get m-args :refresh 0)
        rev (get m-args :reverse 0)
        sp (get m-args :sputter 0)
        sp-amt (get m-args :sputter-amt 0)
        shuffle (get m-args :shuffle 0)]
    (fn
      ([] [size step])
      ([b]
       (let [idx (.indexOf (:def @mem) b)
             beat (if (>= idx 0) (nth (:cur @mem) idx) b)]
         (if (>= b size)
           (do
             (if (weighted-coin refresh)
               (do
                 (if (weighted-coin rev)
                   (swap! mem (fn [m]
                                (assoc m :cur (reverse (:def m))))))
                 (if (weighted-coin sp)
                   (swap! mem (fn [m]
                                (assoc m :cur (sputter (:def m) sp-amt (count (:def m)))))))
                 (if (weighted-coin shuffle)
                   (swap! mem (fn [m]
                                (assoc m :cur (sputter (:def m) sp-amt (count (:def m)))))))
                 )
               (do
                 (swap! mem (fn [m]
                              (assoc m :cur (:def m))))
                 )
               )
             )
           )
         (get base beat))
       )))
  )

(defn phrase-p [inst phrase step & [space args m-args]]
  (let [base (loop [phrase phrase beat 1 pattern {} prev nil]
           (let [args (vec (if (not (nil? args)) args []))
                 note-arg (if (or (instance? overtone.studio.inst.Inst inst)
                                  (instance? overtone.sc.synth.Synth inst))
                            (cond (some #(= (:name %) "freq") (:params inst)) :freq
                                  (some #(= (:name %) "note") (:params inst)) :note
                                  true false))
                 get-note #(if (number? %)
                             (if (and (= note-arg :freq) (< % 100))
                               (midi->hz %)
                               %)
                             (if (= note-arg :freq)
                               (midi->hz (note %))
                               (note %)))
                 mk-action (fn [action block]
                             (reduce
                              (fn [a c]
                                (if (sequential? c)
                                  (conj (vec (butlast a)) (into (last a) c))
                                  (conj a inst (if note-arg (conj args note-arg (get-note c))
                                                   (cons (get-note c) args)))))
                              action
                              (vec block)))
                 cur (first phrase)
                 is-note (or (and (keyword? cur) (nil? (re-find #"^\d" (name cur)))) (number? cur))
                 is-space? #(or (and (sequential? %) (= (first %) :space)) (and (keyword? %) (re-find #"^\d" (name %))))
                 is-arg? #(and (sequential? %) (not (is-space? %)) (keyword? (first %)) (number? (second %)))
                 is-arg (is-arg? cur)
                 is-space (is-space? cur)
                 is-block (and (not is-note) (not is-space) (not is-arg))
                 action (get pattern beat [])
                 action (cond
                          is-note (conj action inst (if note-arg
                                                      (conj args note-arg (get-note cur))
                                                      (cons (get-note cur) args)))
                          is-arg (conj (vec (butlast action)) (into (last action) cur))
                          is-block (mk-action action cur)
                          true nil)
                 pattern (if (and (not (nil? action)) (> (count action) 0))
                           (assoc pattern beat action) pattern)
                 space  (cond is-space (if (sequential? cur) (second cur) (-> cur name Integer/parseInt))
                              (not (nil? space)) space
                              true 0)
                 pattern (if (and (= (count (rest phrase)) 0) (> space 0))
                           (assoc pattern (+ beat (* space step)) [])
                           pattern)
                 beat (if (or (is-arg? (second phrase)) (is-space? (second phrase)))
                        beat
                        (+ beat (* (if (and is-space (nil? prev)) space (inc space)) step))
                        ;(+ beat (* (inc space) step))
                        )
                 beat (if (= (mod beat (int beat)) 0.0) (int beat) beat)]
                                        ;(println "action " action " beat " beat " phrase " (rest phrase) " space " space)
             (if (> (count (rest phrase)) 0)
               (recur (rest phrase) beat pattern cur)
               pattern
               )
             ))]
    (if m-args
      (m-phrase m-args base step)
      base
      )
    )
  )

(defn start-s [synth]
  (if (node-active? synth)
    (ctl synth :listen-bus (get @trigger-buses (to-sc-id synth)))
    )
  synth
  )

(defn play-p [& args]
  "play pattern once for testing"
  (let [s-args (filter number? args)
        speed (if (first s-args) (first s-args) 1)
        patterns (take-while #(not (number? %)) args)
        sizes (mapcat
               #(cond (map? (get-val-if-ref %)) (keys (get-val-if-ref %))
                      (and (fn? (get-val-if-ref %))
                           (= 0 (-> (get-val-if-ref %) class .getDeclaredMethods first .getParameterTypes alength)))
                      (range 1 (first ((get-val-if-ref %))) (second ((get-val-if-ref %))))
                    true [1])
               patterns)
        step (reduce (fn [s b]
                       (min s
                            (loop [s 1.0 b b]
                              (if (some #{(double (mod b (int b)))} (range 0.0 1 s))
                                s
                                (recur (/ s 2) b)))
                            ))
                     1
                     sizes)
        s (get-s speed step true)]
    (ensure-node-active! s)
    (doseq [p patterns]
      (add-p s p))
    (start-s s)
    )
  )

(defn set-size [sequencer size]
  (ctl sequencer :pattern-size size)
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
