(ns techno.sequencer
  (:use [overtone.core]
        [overtone.sc.machinery.server.comms]
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
(defonce ^:private pattern-groups (atom {}))
(defonce ^:private pattern-busses (atom {}))
(defonce ^:private pattern-fx (atom {}))
(defonce ^:private midi-clip (atom {}))
(defonce ^:private  sc-lang (atom (osc-client "127.0.0.1" 57120)))
(defonce ^:private  sc-server (atom (osc-server 4420)))
(defonce ^:private  bus-pool (atom []))
(defonce ^:private  bus-pool-using (atom []))
(defonce  sc-resp (atom nil))
(defonce t-source-g (group "trigger sources"))
(defonce t-synth-g (group "trigger generators" :after t-source-g))

(declare sync-s)

(declare return-bus)

(osc-handle
 @sc-server "/response"
 (fn [m]
   ;(println m)
   (reset! sc-resp m)
   (let [o (read-string (first (:args m)))]
     (when (and (map? o) (contains? o :busses))
       (reset! bus-pool (:busses o)))
     )
   ))

(defn gcd
      [a b]
      (if (zero? b)
      a
      (recur b, (mod a b))))

(defn lcm
      [a b]
      (/ (* a b) (gcd a b)))
;; to calculate the lcm for a variable number of arguments
(defn lcmv [& v] (reduce lcm v))

(defn i-step [offset]
  (if (= (mod offset (int offset)) 0.0) (int offset) offset)
  )

(defn x-seq [coll & [orig]]
  (let [cur (shuffle coll)]
    (lazy-seq
     (cons (first cur)
           (x-seq (if (> (count cur) 1) (rest cur) (shuffle orig))
                  (if (nil? orig) coll orig))))
    )
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
                       )))
  nil)

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

(defn to-str [inst]
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

(defn get-step [pattern]
  (let [val (get-val-if-ref pattern)
        sizes (cond (map? val) (keys val)
                    (and (fn? val)
                         (some #{0}
                               (map #(alength (.getParameterTypes %))
                                    (-> val class .getDeclaredMethods))))
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
(defn p-size [val & [step]]
  "Returns size of a pattern"
  (let [val (get-val-if-ref val)
        step (if step step 0.25)
        p-step (get-step val)
        size (cond
               (fn? val) (if (some #{0}
                                   (map #(alength (.getParameterTypes %))
                                        (-> val class .getDeclaredMethods)))
                           (first (val))
                           1)
               (map? val)  (apply max (keys val))
               (sequential? val) (inc (* (dec (count val)) step))
               true 0
               )
        size (if (and (> p-step step) (map? val) (= 0 (count (get val size))))
               (+ size step)
               size)
        ]
    (if (= (mod size (int size)) 0.0)
      (int size) size)
    ))


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

(defn merge-p [& patterns]
  (let [pat (reduce
          (fn [res cur]
            (let [cur (cond (map? cur) cur
                            (fn? cur) (let [has-size (some #{0}
                                                           (map (fn [f]
                                                                  (alength (.getParameterTypes f)))
                                                                (-> cur class .getDeclaredMethods)))
                                            size (if has-size (first (cur)) (apply max (keys res)))
                                            step (if has-size (second (cur)) 0.25)
                                            offsets (range 1 (+ size step) step)]
                                        (reduce (fn [m o] (if (cur o) (assoc m o (cur o)) m)) {(last offsets) []} offsets)
                                        ))]
              (reduce
               (fn [p b]
                 (let [val (get p b)
                       to-add (get cur b)
                       new-val (if (sequential? val)
                                 (concat val to-add)
                                 to-add)]
                   (assoc p b new-val)))
               res
               (keys cur)))
            )
          {}
          patterns)
        size (apply max (keys pat))]
    (into {}
          (filter (fn [[k v]]
                    (if (or (and (sequential? v) (> (count v) 0))
                            (= size k)
                            ) [k v]))
                  pat)
          )
    )
  )

(defn get-sequencer-data [sequencer]
  (get @sequencer-data (to-sc-id sequencer))
  )

(defn set-size [sequencer size]
  (ctl sequencer :pattern-size size)
  (swap! sequencer-data
         (fn [seq-data]
           (assoc-in seq-data [(to-sc-id sequencer) :size] size)
           ))
  )
(defn set-st [sequencer step]
  (let [old (node-get-control (@trigger-sources (to-sc-id sequencer)) :step)
        size (get-in @sequencer-data [(to-sc-id sequencer) :size])]
      (when (node-active? sequencer)
        (ctl (@trigger-sources (to-sc-id sequencer)) :step step)
        (ctl sequencer :step step)
        (swap! sequencer-data
               (fn [seq-data]
                 (assoc seq-data (to-sc-id sequencer)
                        (assoc (get seq-data (to-sc-id sequencer) {}) :step step))
                 ))
        ;(update-pattern-size sequencer)
        (when (> old step)
            (set-size sequencer
                      (+ size step)))
        )
      )
  )
(defn get-st [sequencer]
  (when (node-active? sequencer)
    (node-get-control sequencer :step)
    )
  )
(defn find-in [coll x]
  (some
   (fn [[k v]]
     (cond (= k x) [k]
           (map? v) (if-let [r (find-in v x)]
                      (into [k] r))))
   coll))
(defn get-action-str [action & [samples sample-var]]
  (let [sample-var (if (not (nil? sample-var)) (symbol sample-var))
        action (vec
                (mapcat
                 (fn [[a arg]]
                   (let [note-arg
                         (if (or (instance? overtone.studio.inst.Inst a)
                                 (instance? overtone.sc.synth.Synth a))
                           (cond (some #(= (:name %) "freq") (:params a)) :freq
                                 (some #(= (:name %) "note") (:params a)) :note
                                 true false) false)
                         arg (if note-arg
                               (loop [arg arg cur (first arg) prev nil final []]
                                 (if (> (count arg) 0)
                                   (recur (rest arg)
                                          (first (rest arg)) cur
                                          (conj final
                                                (if (= prev note-arg)
                                                  (cond (= note-arg :freq) (list 'midi->hz (list 'note (find-note-name (hz->midi cur))))
                                                        (= note-arg :note) (list 'note (find-note-name cur)))
                                                  cur)))
                                   final))
                               arg)
                         ]
                     (vector a arg " "))
                   )
                 (partition 2 action)))
        action (vec (map
                     #(cond (and (not (nil? samples))
                                 (= overtone.sc.sample.PlayableSample (type %)))
                            (list 'get-in sample-var
                                  (find-in samples (keyword
                                                      (clojure.string/replace (:name %) " " ""))))
                            (or (= (type %) overtone.studio.inst.Inst)
                                (= (type %) overtone.sc.synth.Synth)
                                (= overtone.sc.sample.PlayableSample (type %)))
                            (:name %)
                            (sequential? %) (vec %)
                            true %)
                     action))
        action (if (= [] (first action)) [] action)
        action-str (map str action)]
                                        ;(str "[ " (apply str action) " ]")
    (str "[ " (clojure.string/join " " action-str) " ]")
    ))

(defn pp-pattern [pattern]
  (let [pattern (if (fn? pattern) (merge-p pattern) pattern)]
      (when (map? pattern)
        (println "{")
        (doseq [i (sort (keys pattern))]
          (print i " ")
          (doseq [[instrument args] (partition 2 (pattern i))]
            (print "[" (to-str instrument) (to-str args) "]"))
          (println)
          )
        (println "}")))
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
  ([cur-beat pattern] (play cur-beat pattern cur-beat nil))
  ([cur-beat pattern orig-beat] (play cur-beat pattern orig-beat nil))
  ([cur-beat pattern orig-beat p-group] (play cur-beat pattern orig-beat nil nil))
  ([cur-beat pattern orig-beat p-group p-bus]
   (let [data (get-val-if-ref (pattern :data))
         p-bus (if (map? p-bus) (:id p-bus) p-bus)
         beat-actions
         (cond
           (fn? data) (data cur-beat)
           (map? data) (data cur-beat)
           (and (sequential? data))
           (if (<= orig-beat (count data)) (nth data (dec orig-beat))))]
     ;; (println cur-beat)
     ;; (if (and (> (count beat-actions) 0) (fn? (first beat-actions)))
     ;;   (println "playing "
     ;;            (reduce (fn [a b] (str (to-str a) " " (to-str b) " ")) beat-actions)
     ;;            " for beat " cur-beat " raw-beat" orig-beat))
     ;; (when (= cur-beat 1)
     ;;   (println "start"))
     (loop [actions (partition 2 beat-actions) ret false]
       (let [[instrument args] (first actions)
             args (if (not (nil? p-bus)) (concat args [:out-bus p-bus]) args)
             args (if (not (nil? p-group)) (concat [[:head p-group]] args) args)
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
                   (do
                     ;; (println (:name instrument) args)
                     (apply instrument args)
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
    (if (number? seq-size)
      (doseq [[k p] (get @patterns id)]
        ;; (println "orig " beat " stepped beat " stepped-beat " seq-size " seq-size
        ;;          (:size p) " step " step " time " (java.util.Date.)
        ;;          )
        (let [val (get-val-if-ref (p :data))
                                        ;size (p-size val step)
              size (:size p)
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
              play-at-1 (get p :add-at-1 false)
              do-play (or (not play-at-1) (= final-beat 1))
              new-p (if do-play (play final-beat p orig-beat (get @pattern-groups k) (get-in @pattern-fx [k :bus])))]
          (when (and play-at-1 do-play)
            (mod-p synth k :add-at-1 false))
          (swap! sequencer-data
                 (fn [s] (assoc-in s [id :beat] final-beat)))
          (if (map? new-p)
            (swap! patterns
                   (fn [cur]
                     (assoc-in cur [id k] new-p)
                     )))
          ;; (if (or (= k :test)
          ;;          true)
          ;;   (println "playing " k " with beat " final-beat " orig " beat
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

;; (defsynth trigger-source [out-bus 3 clock-speed 2 step 0.25]
;;   (out:kr out-bus (impulse:kr
;;                    (/ clock-speed step)
;;                    ))
;;   )

(defsynth trigger-source [out-bus 3 clock-speed 2 step 0.25 reset-bus -1]
  (out:kr out-bus (t-duty:kr
                   (/ step clock-speed)
                   (in-trig:kr reset-bus)
                   ))
  )

(defsynth midi-clock-source [receive-from 0 send-to 1 step 0.25 div 12]
  (out:kr send-to (pulse-divider:kr (in-trig:kr receive-from) div 1))
  )

(defsynth trigger-synth [listen-bus 3 uid 0 pattern-size 1.75 step 0.25 reset-bus -1]
  (let [trigger (in:kr listen-bus)
        count   (stepper:kr trigger (in-trig:kr reset-bus) :min 1
                            :max (+ 1 (/ (- pattern-size 1) step))
                            :resetval 1)]
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

(defn rm-p
  "(rm-p sequencer pattern) stops the given pattern from being played"
  ([sequencer to-rm] (rm-p sequencer to-rm true))
  ([sequencer to-rm kill-group]
   (doseq [pattern [to-rm]]
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
       (when (and (contains? @pattern-groups pattern) kill-group)
         ;; (kill (get-in @pattern-groups [pattern :id]))
         (when (not (nil? (get-in @pattern-fx [pattern :mixer])))
           (ctl (get-in @pattern-fx [pattern :mixer]) :start-release 1)
           )
         (when (not (nil? (get-in @pattern-fx [pattern :bus])))
           (return-bus (get-in @pattern-fx [pattern :bus]))
           )
         (swap! pattern-groups
              dissoc pattern)
         )
       (when (not (empty? (get patterns id {})))
           (update-pattern-size sequencer))
       )
     )
   )
  )

(defn eval-sc [code]
  (osc-send @sc-lang "/evalCode" code)
  )

(defn alloc-audio-bus []
  (dosync
   (reset! sc-resp {:args nil})
   (eval-sc "b = Bus.audio(s, 2);b.index;")
   @sc-resp
   )
  )

(defn get-bus []
  (let [b (first @bus-pool)]
    (if (not (nil? b))
      (do
        (swap! bus-pool rest)
        (swap! bus-pool-using conj b)
        b)))
  )

(defn return-bus [bus]
  (swap! bus-pool conj bus)
  (swap! bus-pool-using #(filter (fn [i] (not (= i bus))) %))
  )

(defn free-audio-bus [bus]
  (dosync
   (eval-sc (str "b = Bus.new('audio', " bus ", 2);b.free;"))
   )
  )

(defn bulk-alloc-busses []
  (dosync
   (eval-sc (str "~busses=11.collect{b = Bus.audio(s, 2); b.index;};\"{:busses \" + ~busses.asString + \"}\"; "))
   )
  )

(defn reset-busses []
  (eval-sc "(4..1000).do{|i| Bus.new('audio', i, 2, s).free};")
  )

(when (nil? (first @bus-pool))
  (bulk-alloc-busses))

(defsynth p-mixer [audio-bus 10 out-bus 0 volume 1 start-release 0]
      (let [source    (in:ar audio-bus 2)
            source    (* volume source)]
        (detect-silence:ar (select:ar start-release [(dc:ar 1) source]) :action 14)
        (out:ar 0 source)
        ))

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
         (rm-p sequencer key false))
       (swap! patterns
              (fn [p]
                (assoc p id
                       (assoc cur-val key
                              (merge {:data pattern
                                      :size (p-size pattern (get-st sequencer))} attrs))
                       )))
       (when  (and (not (get-in @sequencer-data [id :one-shot] false))
                   (not (contains? @pattern-groups key))
                   (not (contains? attrs :no-group)))
         (let [vol (get attrs :volume 1)
               p-group (group)
               p-bus (get-bus)
               mixer (p-mixer [:tail p-group] p-bus :volume vol)]
           (swap! pattern-groups assoc key p-group)
           (when (not (nil? p-bus))
             (swap! pattern-fx assoc-in [key :mixer] mixer)
             (swap! pattern-fx assoc-in [key :bus] p-bus))
           )
         )
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
       (event ::pattern-added :data pattern)
       )))
  )

(defn get-pattern-fx [key]
  (get @pattern-fx key)
  )

(defn add-pattern-fx [pat key nod]
  (swap! pattern-fx assoc-in [pat key] nod)
  )

(defn rm-pattern-fx [pat key]
  (swap! pattern-fx
         (fn [p]
           (if (node? (get-in p [pat key]))
               (kill (get-in p [pat key])))
           (assoc p pat (dissoc (get p pat) key))))
  )


(defn handle-pattern-fx [key attrs kill-group & [fx]]
  (when (and (contains? @pattern-groups key) kill-group)
    ;; (kill (get-in @pattern-groups [pattern :id]))
    (when (not (nil? (get-in @pattern-fx [key :mixer])))
      (try
        (ctl (get-in @pattern-fx [key :mixer]) :start-release 1)
        (catch Exception e
          (kill (get-in @pattern-groups [key :id]))))
      )
    (when (not (nil? (get-in @pattern-fx [key :bus])))
      (return-bus (get-in @pattern-fx [key :bus]))
      )
    (swap! pattern-groups
           dissoc key)
    (swap! pattern-fx
           dissoc key)
    )
  (when  (and (not kill-group)
              (not (contains? @pattern-groups key))
              (not (contains? attrs :no-group)))
    (let [vol (get attrs :volume 1)
          p-group (group)
          p-bus (get-bus)
          mixer (p-mixer [:tail p-group] p-bus :volume vol)]
      (swap! pattern-groups assoc key p-group)
      (when (not (nil? p-bus))
        (swap! pattern-fx assoc-in [key :mixer] mixer)
        (swap! pattern-fx assoc-in [key :bus] p-bus)
        (swap! pattern-fx assoc-in [key :group] p-group))
      )
    )
  (doseq [[f x] (get @pattern-fx key)]
    (if (and (not (= f :mixer)) (not (= f :group))
             (node-active? x))
      (kill x)))
  (when (and (map? fx) (contains? @pattern-groups key))
    (let [p-group (get @pattern-groups key)
          p-bus (get-in @pattern-fx [key :bus])]
        (doseq [[k v] fx]
          (swap! pattern-fx assoc-in [key k]
                 (apply (first v)
                        (concat
                         [[:head p-group] :audio-bus p-bus :out-bus p-bus]
                         (rest v))))))
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

     (free-bus (get-in @sequencer-data [id :reset-bus]))
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

(defn build-sequencer
  ([source-synth bus] (build-sequencer source-synth bus 0.25 false))
  ([source-synth bus step one-shot] (build-sequencer source-synth bus 0.25 false -1))
  ([source-synth bus step one-shot reset-bus]
   (let [uid (trig-id)
         listen-to (if one-shot 3 bus)
         synth (trigger-synth [:tail t-synth-g] listen-to uid 1.75 step reset-bus)
         key (keyword (gensym "sequencer"))]
     (swap! trigger-buses (fn [buses]
                            (assoc buses (to-sc-id synth) bus)
                            ))
     (swap! trigger-sources (fn [sources]
                              (assoc sources (to-sc-id synth) source-synth)
                              ))
     (swap! sequencer-data assoc-in [(to-sc-id synth) :uid] uid)
     (swap! sequencer-data assoc-in [(to-sc-id synth) :reset-bus] reset-bus)
     (swap! sequencer-data assoc-in [(to-sc-id synth) :one-shot] one-shot)
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
   (let [trigger-bus (control-bus)
         reset-bus (control-bus)]
     (build-sequencer
      (trigger-source [:tail t-source-g] trigger-bus clock-speed step reset-bus)
      trigger-bus
      step
      one-shot
      reset-bus)))
  )

(defn midi-s [midi-bus]
  (let [trigger-bus (control-bus)]
    (build-sequencer
     (midi-clock-source [:tail t-source-g] midi-bus trigger-bus :div 6)
     trigger-bus
     0.25
     false)))

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

(defn reset-s [player]
  (let [bus (get-in @sequencer-data [(to-sc-id player) :reset-bus])]
    (control-bus-set! bus 1)
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
   (get @patterns (to-sc-id sequencer) {}))
  ([sequencer pattern]
   (get-in @patterns [(to-sc-id sequencer) pattern] {}))
  )

(defn set-sp [sequencer speed]
  (ctl (get-source sequencer) :clock-speed speed)
  )

(defn get-sp [sequencer]
  (node-get-control (@trigger-sources (to-sc-id sequencer)) :clock-speed)
  )


(defn mod-actions [sequencer pattern f]
  (if (and (contains? (get @patterns (to-sc-id sequencer)) pattern)
           (map? (get-val-if-ref (get-in @patterns [(to-sc-id sequencer) pattern :data]))))
      (swap! patterns (fn [p]
                        (let [id (to-sc-id sequencer)
                              key (if (keyword? pattern) pattern
                                      (first (first
                                              (filter (fn [[k v]]
                                                        (= (v :data)  pattern)) (p id)))))
                              cur-data (get-in p [id key :data])
                              is-atom (instance? clojure.lang.Atom cur-data)
                              data (get-val-if-ref (if cur-data cur-data {}))
                              is-map (map? data)
                              offsets (filter #(sequential? (get data %)) (keys data))]
                          (if (and (not (nil? key)) is-map)
                            (let [res (reduce (fn [m k]
                                                (assoc m k
                                                       (let [actions (get data k)]
                                                         (mapcat f (partition 2 actions))
                                                         )))
                                              data offsets)]
                              (if is-atom
                                (do (swap! cur-data (fn [_] res))
                                    p)
                                (assoc-in p [id key :data]
                                          res)))
                            p)
                          ))))
  nil)

(defn set-amp [sequencer pattern amp]
  (mod-actions
   sequencer pattern
   (fn [[inst args]]
     (let [args (vec (mapcat #(if (not (= :amp (first %))) % []) (partition 2 args)))]
       [inst (conj args :amp amp)])))
  )

(defn mod-amp [sequencer pattern delta]
  (if (not (nil? (get-in @pattern-fx [pattern :mixer])))
    (let [cur (+ delta (node-get-control (get-in @pattern-fx [pattern :mixer]) :volume))
          cur (if (<= cur 0) 0 cur)]
        (ctl (get-in @pattern-fx [pattern :mixer]) :volume cur))
      ;; (mod-actions
      ;;  sequencer pattern
      ;;  (fn [[inst args]]
      ;;    (let [arg-map (into {} (map vec (partition 2 args)))
      ;;          amp (if (contains? arg-map :amp) (:amp arg-map) 0.8)
      ;;          args (vec (mapcat #(if (not (= :amp (first %))) % []) (partition 2 args)))]
      ;;      [inst (conj args :amp (+ amp delta))])))
      )
  )

(defn set-arg [sequencer pattern arg val]
  (mod-actions
   sequencer pattern
   (fn [[inst args]]
     (let [args (vec (mapcat #(if (not (= arg (first %))) % []) (partition 2 args)))]
       [inst (conj args arg val)])))
  )


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

(defn build-map-p [pattern & [step]]
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
  )

(defn build-rest-p [pattern & [step samples sample-var]]
  (let [step (if step step (get-step pattern))
        size (p-size pattern step)
        offsets (sort (filter #(let [a (get pattern %)]
                         (or (and (sequential? a) (not (nil? (first a))))
                             (= 1 %) (= (double size) %)))
                              (map (fn [o] (if (= (mod o (int o)) 0.0)
                                            (int o) o)) (range 1 (+ size step) step))))]
    (loop [p [] beats offsets prev (first offsets) cur (first offsets) next (second offsets)]
      (let [cur-action (get pattern cur)
            is-head (= 1 cur)
            is-action (and (sequential? cur-action) (not (nil? (first cur-action))))
            is-next-action (and (sequential? (get pattern next))
                                (not (nil? (first (get pattern next)))))
            has-rest (and (number? next) (> (- next cur) step))
            conj-args (if (not (nil? cur))
                          (cond (and has-rest is-action is-next-action)
                                (vector p cur-action
                                        (keyword (str (int (dec (/ (- next cur) step))))))
                                (and is-head (not is-action))
                                (vector p (keyword (str (int (/ (- next cur) step)))))
                                (not is-action) ;is tail
                                (vector
                                 p
                                 (keyword (str (int (/ (- cur prev) step)))))
                                true (vector p cur-action)))]
        (if (<= (count beats) 0)
          p
          (recur (apply conj conj-args)
                 (rest beats)
                 cur
                 next
                 (second (rest beats)))))
      ))
  )

(defn build-phrase-p [pattern]
  (let [action (first (filter #(not (nil? (first %))) (vals pattern)))
        inst (first action)
        params (vec (flatten (into [] (filter (fn [[k v]] (and (not (= k :note)) (not (= k :freq)))) (partition 2 (second action))))))
        rest-p (build-rest-p pattern)
        step (get-step pattern)
        phrase-p (vec (map
                       #(if (sequential? %)
                          (let [a (map (fn [[a params]]
                                       (let [m (into {} (vec (map vec (partition 2 params))))]
                                         (cond (contains? m :freq) (find-note-name (hz->midi (:freq m)))
                                               (contains? m :note) (find-note-name (:note m)))
                                         )) (partition 2 %))]
                            (if (> (count a) 1)
                              (vec a)
                              (first a))
                            )
                          %) rest-p))]
   [inst phrase-p step params]
    )
  )

(defn is-phrase? [pattern]
  (let [insts (reduce
               (fn [c a]
                 (reduce
                  (fn [c [i p]]
                    (if (or (instance? overtone.studio.inst.Inst i)
                            (instance? overtone.sc.synth.Synth i)
                            (instance? overtone.sc.sample.PlayableSample i))
                      (assoc c (:name i) i)
                      c))
                  c
                  (partition 2 a))
                 )
               {}
               (vals pattern))
        inst (first (vals insts))
        note-arg (if (or (instance? overtone.studio.inst.Inst inst)
                         (instance? overtone.sc.synth.Synth inst))
                   (cond (some #(= (:name %) "freq") (:params inst)) :freq
                         (some #(= (:name %) "note") (:params inst)) :note
                         true false))
        actions (first (filter #(and (sequential? %) (not (nil? (first %)))) (vals pattern)))
        has-note (not (= -1 (.indexOf (second actions) note-arg)))]
    (and (= (count insts) 1) has-note)
    )
  )

(defn p-shift [pattern shift-by]
  (let [to-int #(if (= (mod % (int %)) 0.0)
                  (int %) %)
        step (get-step pattern)
        size (p-size pattern step)
        n-pattern (if (map? pattern) (map #(get pattern (to-int %) nil) (range 1 (+ size step) step))
                       pattern)
        n-pattern (if (sequential? n-pattern)
                    (take (count n-pattern)
                           (drop (if (pos? shift-by) (- (count n-pattern) shift-by) (* -1 shift-by)) (cycle n-pattern)))
                    pattern)]
    (if (map? pattern)
      (build-map-p n-pattern)
      n-pattern)
    )
  )

(defn arp-p [in notes & [args space reps]]
  "build arpeggio (arp-p bass [60 62 66] [:amp 0.5] & space)"
  (build-map-p
       (apply concat
              (map #(vector [in (cons % (if (nil? args) [] args))]
                            [:space (if (nil? space) 1 space)]
                            )
                   (apply concat (repeat (if (nil? reps) 1 reps) notes))
                   )
              )
       )
  )



(defn dump-p [sequencer & parts]
  (let [data (get @patterns (to-sc-id sequencer))
        parts (if parts parts (keys data))
        to-fit {(get-in @sequencer-data [(to-sc-id sequencer) :size]) []}
        p (apply merge-p (map #(get % :data) (conj (vals (select-keys data parts)) to-fit)))]
    (pp-pattern p))
  )


(defn stretch-p
  ;; ([sequencer pattern new-size]
  ;;  (swap! patterns
  ;;         (fn [p]
  ;;           (assoc-in p
  ;;                     [(to-sc-id sequencer) pattern :data]
  ;;                     (stretch-p (get-in @patterns [(to-sc-id sequencer) pattern :data]) new-size))))
  ;;  nil)
  ([pattern new-size & [step]]
   (let [stretched {new-size []}
         step (if step step (get-step pattern))
         steps (map i-step (range 1 (+ new-size step) step))
         orig-steps (cycle (map i-step (range 1 (+ (apply max (keys pattern)) step) step)))
         stretched (reduce
                    (fn [p o]
                      (let [w (nth orig-steps (int (/ (dec o) step)))]
                        (if (contains? pattern w)
                          (assoc p o (get pattern w))
                          p)))
                    stretched
                    steps)]
     stretched
     )
   ;; (let [size-arg new-size
   ;;       step (get-step pattern)
   ;;       tail (apply max (if (> (count (keys pattern)) 0) (keys pattern) [1]))
   ;;       quantize #(+ 1 (/ (- % 1) step))
   ;;       new-map (zipmap (map quantize (keys pattern)) (vals pattern))
   ;;       size (quantize tail)
   ;;       new-size (quantize (if new-size new-size (* tail 2)))
   ;;       beats (cycle (range 1 (inc size)))]
   ;;   (if (>= new-size size)
   ;;       (reduce
   ;;        (fn [p b]
   ;;          (let [cur-beat (+ 1 (* (- b 1) step))
   ;;                cur-beat (if (= (double (mod cur-beat (int cur-beat))) 0.0) (int cur-beat) cur-beat)
   ;;                wrapped (double (nth beats (dec b)))
   ;;                action (get new-map wrapped)]
   ;;            (if (or action (= b new-size))
   ;;              (assoc p cur-beat action)
   ;;              p))
   ;;          ) pattern (range (inc size) (inc new-size)))
   ;;       (let [pattern (apply dissoc pattern (filter #(> % size-arg) (keys pattern)))]
   ;;         ))
   ;;   )
   )
  )

(defn fit-p [base pattern & [fill pad step]]
  (let [step (if step step (get-step pattern))
        base-size (inc (/ (dec (p-size base step)) step))
        size (inc (/ (dec (p-size pattern step)) step))
        pad (if pad pad 0)
        size (loop [size size] (if (= (double (mod size base-size)) 0.0) size (recur (inc size))))
        size (+ size (* base-size pad))
        size (inc (* (dec size) step))
        pattern (if fill
                  (stretch-p pattern size)
                  (if (contains? pattern size)
                    pattern
                    (assoc pattern
                           (if (= (double (mod size (int size))) 0.0) (int size) size)
                           [])))]
    pattern
    )
  )

(defn set-action [sequencer pattern key new & [stretch]]
  (if (and (contains? (get @patterns (to-sc-id sequencer)) pattern)
           (map? (get-val-if-ref (get-in @patterns [(to-sc-id sequencer) pattern :data]))))
    (swap! patterns (fn [p]
                      (let [id (to-sc-id sequencer)
                            entry (get-in p [id pattern])
                            cur-data (get entry :data)
                            is-atom (instance? clojure.lang.Atom cur-data)
                            data (get-val-if-ref (if cur-data cur-data {}))
                            data (if (and stretch (> key (p-size data)))
                                   (fit-p data (assoc (stretch-p data key) key new) true)
                                   (assoc data key new))
                            entry (assoc (assoc entry :data data) :size (p-size data))]
                        (if (not (nil? key))
                          (if is-atom
                            (do (reset! cur-data data)
                                (assoc-in p [id pattern] entry))
                            (assoc-in p [id pattern]
                                      entry))
                          p)
                        ))))
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
  (let [;inst (if (sequential? inst) inst [inst])
        base
        (loop [phrase phrase beat 1 pattern {} prev nil]
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
                         is-note
                         ;(apply conj (concat [action] (mapcat #() inst)))
                         (conj action inst (if note-arg
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
        times (if (second s-args) (second s-args) 1)
        patterns (take-while #(not (number? %)) args)
        sizes (mapcat
               #(cond (map? (get-val-if-ref %)) (keys (get-val-if-ref %))
                      (and (fn? (get-val-if-ref %))
                           (some #{0}
                                 (map (fn [f] (alength (.getParameterTypes f)))
                                      (-> % class .getDeclaredMethods)))
                           )
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
    (do
      (doseq [p patterns]
        (add-p s p))
      (let [size (get-in @sequencer-data [(to-sc-id s) :size])
            size (+ 1 (/ (- size 1) step))
            size (+ 1 (* (- (* times size) 1) step))]
        (set-size s size)
        ))
    (start-s s)
    )
  )

(defn p-fx
  ([pattern] (get @pattern-fx pattern))
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
;; (
;; f = { |msg, time, replyAddr, recvPort|
;; 	b = NetAddr.new("127.0.0.1", 4420);
;;     if(msg[0] == '/evalCode') {
;; 		b.sendMsg("/response", msg[1].asString.interpretPrint);
;;     }
;; };
;; );

;; thisProcess.addOSCRecvFunc(f);
;; thisProcess.removeOSCRecvFunc(f);
