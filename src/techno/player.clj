(ns techno.player
  (:use [overtone.core :exclude [now stop show-schedule]]
        ;[techno.synths]
        )
  (:import [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit ThreadPoolExecutor]
           [java.io Writer]))

(defrecord PoolInfo [thread-pool jobs-ref id-count-ref])
(defrecord MutablePool [pool-atom])
(defrecord RecurringJob [id ms-period
                         job pool-info
                         scheduled? state counter])

(defonce pool (atom nil))
(defonce patterns (atom {}))
(declare mk-pool)
(declare play)
(declare p-size)
(declare set-size)
(declare stop-s)
(declare schedule-job)
(declare now2)
(declare update-size)
(declare handle-beat-trigger)
(declare shutdown-pool-now!)

(defn get-val-if-ref [x]
  (if (instance? clojure.lang.Atom x)
    @x
    x
    )
  )

(defn get-pos [beat div]
       (let [bar (if (= 0 (mod beat div)) (/ beat div) (inc (int (/ beat div))))
             n (mod beat div)
             n (if (= 0 n) div n)]
         [(int bar) (int n)]))

(defn find-in [coll x]
  (some
   (fn [[k v]]
     (cond (= k x) [k]
           (map? v) (if-let [r (find-in v x)]
                      (into [k] r))))
   coll))
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

(defn- get-job [id]
  (get @(:jobs-ref @pool) id)
  )

(defn- to-str [inst]
  (if (and (coll? inst) (contains? inst :name))
    (:name inst)
    inst
    ))

(defn get-s
  ([bpm & [options]]
   (if (or (nil? @pool) (.isShutdown (:thread-pool @pool)))
     (swap! pool (fn [_] (mk-pool))))
   (let [div (get options :div 4)
         size (get options :size 4)
         period (/ 60000 bpm div)
         args (if (not (nil? options)) options {})
         job (schedule-job @pool
                           play
                           period
                           (merge {:bpm bpm :size size :div div}
                                  args)
                           (ref 1))]
     (:id job)
     ))
  )

(defn p-size [p & [div]]
  (let [b (apply max (filter number? (keys p)))
        s (* (get p :div) (dec b))
        step (if (not (nil? div)) (/ div (get p :div)) 1)
        s (+ s (* step (if (not (empty? (get p b)))
                         (apply max (keys (get p b)))
                         1)))]
    s
    )
  )
(defn- update-player [id]
  (try
    (let [state (:state (get @(:jobs-ref @pool) id))
          divs (conj (map #(get % :div) (vals (get @patterns id))) (:div @state))
          div (apply lcmv divs)
          bpm (:bpm @state)
          size (apply max
                      (map
                       (fn [[k p]]
                         (let [b (apply max (filter number? (keys p)))
                               s (* div (dec b))
                               step (/ div (get p :div))
                               s (+ s (* step
                                         (if (not (empty? (get p b)))
                                           (apply max (keys (get p b)))
                                           1)))]
                           (swap! patterns assoc-in [id k :size] s)
                           s
                           ))
                       (get @patterns id)))]
      (when (or (not (= 0 (mod div (:div @state)))) (> div (:div @state)))
        (stop-s id true false)
        (get-s bpm {:id id :div div :size size}))
      (set-size id size)
      )
    (catch Exception e
      (println (str "caught exception: " (.getMessage e))))
    )
  )


(defn play [id]
  (try
    (dosync
     (let [state (:state (get @(:jobs-ref @pool) id))
           counter (:counter (get @(:jobs-ref @pool) id))
           beat @counter
           size (:size @state)
           s-div (:div @state)]
       ;; (println "beat " beat)
       (doseq [[k v] (get @patterns id)]
         (let [beat (if (< (:size v) beat) (mod beat (:size v)) beat)
               div (get v :div)
               step (/ s-div div)]
           (when (or (= 1 beat) (= 0 (mod (dec beat) step)))
             (let [bar (inc (int (/ (dec beat) s-div)))
                   note (inc (int (/ (dec (mod beat s-div)) step)))
                   note (if (= 0 note) div note)]
               ;; (println "k " k "bar " bar "note " note)
               (doseq [[a args] (partition 2 (get-in v [bar note]))]
                 (apply a args)
                 ))
             ))
         )
       (if (>= beat size)
         (ref-set counter 1)
         (commute counter inc))
       ))
    (catch Exception e (println (.getMessage e))
           (.printStackTrace e)))
  )
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
    (str "[" (clojure.string/join " " action-str) "]")
    ))
(defn pp-pattern [pattern]
  (let [p (into (sorted-map) (into {} (filter #(number? (first %)) pattern)))
        bars (keys p)
        bar (if (not (empty? bars)) (apply max bars) 1)
        note (if (not (empty? (get pattern bar)))
               (apply max (keys (get pattern bar)))
               1)
        p (if (empty? (get-in pattern [bar note])) (assoc-in p [bar note] [:end]) p)
        notes (map #(into (sorted-map) %) (vals p))
        notes (map (fn [a b] (assoc (into {} (map (fn [[k v]] [k
                                                             (get-action-str v)
                                                             ]) a)) 0 b)) notes bars)]
    (clojure.pprint/print-table (range 0 (inc (:div pattern))) notes)
    )
  )
;; (def p (get-s 120 {:div 16}))
;; (add-p p
;;        (phrase-p
;;         bpfsaw [:Eb4 :C4 :6 :G4 :2 :Bb4 :Ab4 :1 :F3] 1/8 0 [:dur 0.9 :atk 0.01 :rq 0.5]) :test)
;; (add-p p
;;        (phrase-p
;;         bass-synth [:Eb4 :C4 :6 :G4 :2 :Bb4 :Ab4 :1 :F3] 1/4 0 [:attack 0.01 :release 0.4 :bwr 2]) :test2)
;; (add-p p
;;        (build-map-p [[o-kick []] :3] 1/4) :kick)
;; (let [h [o-hat []]]
;;     (add-p p
;;            (build-map-p [h h h :2 h h h :4 h h h :1] 1/16) :hat))
;; (stop-s p)

(defn add-p [id pattern key]
  (swap! patterns assoc-in [id key] pattern)
  (update-player id)
  )


(defn set-size [id size]
  (swap! (:state (get-job id)) assoc :size size)
  )

(defn set-st [id step]
  (swap! (:state (get-job id)) assoc :step step)
  )
(defn set-sp [id speed]
  (let [step (:step @(:state (get-job id)))]
    (stop-s id true)
    (get-s speed {:id id})
   )
  )

(defn build-map-p [pattern & [div]]
  (let [div (if div (int (/ 1 div)) 4)]
    (loop [p {:div div} pattern pattern beat 1 start true]
      (let [a (first pattern)
            pos (get-pos beat div)
            pattern (rest pattern)
            end? (empty? pattern)
            beat (+ beat
                    (cond
                      (keyword? (first pattern)) 0
                      (keyword? a) (if (or start end?) (-> a name Integer/parseInt)
                                       (inc (-> a name Integer/parseInt)))
                      true 1))
            p (cond (sequential? a) (assoc-in p pos a)
                    (and end? (keyword? a)) (assoc-in p (get-pos beat div) [])
                    true p)]
        (if end?
          p
          (recur p pattern beat false))
        )
      )
    )
  )

(defn phrase-p [inst pattern div & [space args]]
  (let [note-arg (if (or (instance? overtone.studio.inst.Inst inst)
                         (instance? overtone.sc.synth.Synth inst))
                   (cond (some #(= (:name %) "freq") (:params inst)) :freq
                         (some #(= (:name %) "note") (:params inst)) :note
                         true false))
        note-p #(do ;%
                  (if (= note-arg :freq) (midi->hz (note %)) (note %))
                  )
        mk-note (fn [n & [n-args]] (vector inst (vec (concat [note-arg (note-p n)] (if n-args n-args args)))))
        is-space? #(and (keyword? %) (re-find #"^\d" (name %)))
        is-note? #(or (and (keyword? %) (re-find #"^\w" (name %))) (and (sequential? %) (fn? (first %))))
        is-arg? #(and (sequential? %) (or (number? (first %)) (number? (second %))))]
    (build-map-p (map (fn [a] (cond (sequential? a)
                                   (vec (mapcat #(cond (is-arg? (second %)) (mk-note (first %) (second %))
                                                       (is-note? (second %)) (vec (concat (mk-note (first %)) (mk-note (second %))))
                                                       true (mk-note (first %)))
                                                (partition 2 2 [args] a)))
                                   (is-space? a) a
                                   true (mk-note a)))
                      pattern)))
  )

;; (defn phrase-p [inst pattern div & [space args]]
;;   (let [space (if space space 0)
;;         note-arg (if (or (instance? overtone.studio.inst.Inst inst)
;;                          (instance? overtone.sc.synth.Synth inst))
;;                    (cond (some #(= (:name %) "freq") (:params inst)) :freq
;;                          (some #(= (:name %) "note") (:params inst)) :note
;;                          true false))
;;         note-p #(do ;%
;;                   (if (= note-arg :freq) (midi->hz (note %)) (note %))
;;                   )
;;         is-space? #(and (keyword? %) (re-find #"^\d" (name %)))
;;         is-note? #(or (and (keyword? %) (re-find #"^\w" (name %))) (and (sequential? %) (fn? (first %))))
;;         is-arg? #(and (sequential? %) (or (number? (first %)) (number? (second %))))
;;         div (int (/ 1 div))
;;         get-pos (fn [beat]
;;                   (let [bar (if (= 0 (mod beat div)) (/ beat div) (inc (int (/ beat div))))
;;                         n (mod beat div)
;;                         n (if (= 0 n) div n)]
;;                     [bar n]))]
;;       (loop [p {:div div} pattern pattern beat 1]
;;         (let [mk-note (fn [n & [n-args]] (vector inst (vec (concat [note-arg (note-p n)] (if n-args n-args args)))))
;;               a (first pattern)
;;               pattern (rest pattern)
;;               [bar n] (get-pos beat)
;;               d (cond (is-space? a)
;;                       (if (is-note? (first pattern))
;;                         (inc (-> a name Integer/parseInt))
;;                         (-> a name Integer/parseInt))
;;                       (or ;(is-space? (first pattern))
;;                           (and (is-arg? (first pattern)) (is-space? (second pattern)))) 0
;;                       true (+ space 1))
;;               old-beat beat
;;               beat (+ beat d)
;;               ;x (println "a " a "beat " beat "pos " (get-pos beat))
              ;; a (cond (sequential? a) (vec (mapcat #(cond (is-arg? (second %))
              ;;                                             (mk-note (first %) (second %))
              ;;                                             (is-note? (second %))
              ;;                                             (vec (concat (mk-note (first %)) (mk-note (second %))))
              ;;                                             true (mk-note (first %)))
              ;;                                      (partition 2 2 [args] a)))
;;                       (is-space? a) []
;;                       true (if (is-arg? (first pattern)) (mk-note a (first pattern)) (mk-note a)))
;;               pattern (if (is-arg? (first pattern)) (rest pattern) pattern)
;;               end? (= (count pattern) 0)
;;               p (cond (not (= a [])) (assoc-in p [bar n] a)
;;                       (and end? (> d 0)) (assoc-in p (get-pos beat) [])
;;                       true p)
;;               p (if (and end? (not (is-space? a)) (> space 0)) (assoc-in p (get-pos (+ old-beat space)) []) p)]
;;           (if end?
;;             p
;;             (recur p pattern beat))
;;           )))
;;   )


(defn- format-date
  "Format date object as a string such as: 15:23:35s"
  [date]
  (.format (java.text.SimpleDateFormat. "EEE hh':'mm':'ss's'") date))

(defmethod print-method PoolInfo
  [obj ^Writer w]
  (.write w (str "#<PoolInfo: " (:thread-pool obj) " "
                 (count @(:jobs-ref obj)) " jobs>")))

(defmethod print-method MutablePool
  [obj ^Writer w]
  (.write w (str "#<MutablePool - "
                 "jobs: "(count @(:jobs-ref @(:pool-atom obj)))
                 ">")))

(defmethod print-method RecurringJob
  [obj ^Writer w]
  (.write w (str "#<RecurringJob id: " (:id obj)
                 ", ms-period: " (:ms-period obj)
                 ", initial-delay: " (:initial-delay obj)
                 ", scheduled? " @(:scheduled? obj) ">")))


(defn- switch!
  "Sets the value of atom to new-val. Similar to reset! except returns the
  immediately previous value."
  [atom new-val]
  (let [old-val  @atom
        success? (compare-and-set! atom old-val new-val)]
    (if success?
      old-val
      (recur atom new-val))))

(defn- cpu-count
  "Returns the number of CPUs on this machine."
  []
  (.availableProcessors (Runtime/getRuntime)))



(defn- schedule-job
  "Schedule the fun to execute periodically in pool-info's pool with the
  specified initial-delay and ms-period. Returns a RecurringJob record."
  [pool-info fun ms-period job-args counter]
  (let [ms-period     (long ms-period)
        ^ScheduledThreadPoolExecutor t-pool (:thread-pool pool-info)
        start-time    (System/currentTimeMillis)
        jobs-ref      (:jobs-ref pool-info)
        id-count-ref  (:id-count-ref pool-info)]
    (dosync
     (let [id       (cond (contains? job-args :id) (:id job-args)
                      true (commute id-count-ref inc))
           job  (.scheduleAtFixedRate t-pool
                                      #(fun id)
                                      0
                                      ms-period
                                      TimeUnit/MILLISECONDS)
           job-info (map->RecurringJob {:id id
                                        :ms-period ms-period
                                        :job job
                                        :pool-info pool-info
                                        :scheduled? (atom true)
                                        :state (atom job-args)
                                        :counter counter})]
       (commute jobs-ref assoc id job-info)
       job-info))))



(defn shutdown []
  (if (and (not (nil? @pool))
           (not (.isShutdown (:thread-pool @pool))))
    (shutdown-pool-now! @pool)
    )
  )

(defn- shutdown-pool-now!
  "Shut the pool down NOW!"
  [pool-info]
  (.shutdownNow (:thread-pool pool-info))
  (doseq [job (vals @(:jobs-ref pool-info))]
    (reset! (:scheduled? job) false)))

(defn- shutdown-pool-gracefully!
  "Shut the pool down gracefully - waits until all previously
  submitted jobs have completed"
  [pool-info]
  (.shutdown (:thread-pool pool-info))
  (let [jobs (vals @(:jobs-ref pool-info))]
    (future
      (loop [jobs jobs]
        (doseq [job jobs]
          (when (and @(:scheduled? job)
                     (or
                      (.isCancelled (:job job))
                      (.isDone (:job job))))
            (reset! (:scheduled? job) false)))

        (when-let [jobs (filter (fn [j] @(:scheduled? j)) jobs)]
          (Thread/sleep 500)
          (when (seq jobs)
            (recur jobs)))))))

(defn- mk-sched-thread-pool
  "Create a new scheduled thread pool containing num-threads threads."
  [num-threads]
  (let [t-pool (ScheduledThreadPoolExecutor. num-threads)]
    t-pool))

(defn- mk-pool-info
  [t-pool]
  (PoolInfo. t-pool (ref {}) (ref 0N)))

(defn mk-pool
  "Returns MutablePool record storing a mutable reference (atom) to a
  PoolInfo record which contains a newly created pool of threads to
  schedule new events for. Pool size defaults to the cpu count + 2."
  [& {:keys [cpu-count stop-delayed? stop-periodic?]
      :or {cpu-count (+ 2 (cpu-count))}}]
  (mk-pool-info (mk-sched-thread-pool cpu-count))
  ;(MutablePool. (atom (mk-pool-info (mk-sched-thread-pool cpu-count))))
  )


(defn- shutdown-pool!
  [pool-info strategy]
  (case strategy
    :stop (shutdown-pool-gracefully! pool-info)
    :kill (shutdown-pool-now! pool-info)))

(defn stop-and-reset-pool!
  "Shuts down the threadpool of given MutablePool using the specified
  strategy (defaults to :stop). Shutdown happens asynchronously on a
  separate thread.  The pool is reset to a fresh new pool preserving
  the original size.  Returns the old pool-info.

  Strategies for stopping the old pool:
  :stop - allows all running and scheduled tasks to complete before
          waiting
  :kill - forcefully interrupts all running tasks and does not wait

  Example usage:
  (stop-and-reset-pool! pool)            ;=> pool is reset gracefully
  (stop-and-reset-pool! pool
                        :strategy :kill) ;=> pool is reset forcefully"
  [pool & {:keys [strategy]
           :or {strategy :stop}}]
  (when-not (some #{strategy} #{:stop :kill})
    (throw (Exception. (str "Error: unknown pool stopping strategy: " strategy ". Expecting one of :stop or :kill"))))
  (let [pool-atom pool
        ;pool-atom      (:pool-atom pool)
        ^ThreadPoolExecutor tp-executor (:thread-pool @pool-atom)
        num-threads   (.getCorePoolSize tp-executor)
        new-t-pool    (mk-sched-thread-pool num-threads)
        new-pool-info (mk-pool-info new-t-pool)
        old-pool-info (switch! pool-atom new-pool-info)]
    (future (shutdown-pool! old-pool-info strategy))
    old-pool-info))

(defn- cancel-job
  "Cancel/stop scheduled fn if it hasn't already executed"
  [job-info cancel-immediately?]
  (if (:scheduled? job-info)
    (let [job       (:job job-info)
          id        (:id job-info)
          pool-info (:pool-info job-info)
          pool      (:thread-pool pool-info)
          jobs-ref  (:jobs-ref pool-info)]
      (.cancel job cancel-immediately?)
      (reset! (:scheduled? job-info) false)
      (dosync
       (let [job (get @jobs-ref id)]
         (commute jobs-ref dissoc id)
         (true? (and job (nil? (get @jobs-ref id))))))) ;;return true if success
    false))

(defn- cancel-job-id
  [id pool cancel-immediately?]
  (let [pool-info @pool
        ;pool-info @(:pool-atom pool)
        jobs-info @(:jobs-ref pool-info)
        job-info (get jobs-info id)]
    (cancel-job job-info cancel-immediately?)))

(defn stop2
  "Stop a recurring or scheduled job gracefully either using a
  corresponding record or unique id. If you specify an id, you also
  need to pass the associated pool."
  ([job] (cancel-job job false))
  ([id pool] (cancel-job-id id pool false)))

;; (defn kill
;;   "kill a recurring or scheduled job forcefully either using a
;;   corresponding record or unique id. If you specify an id, you also
;;   need to pass the associated pool."
;;   ([job] (cancel-job job true))
;;   ([id pool] (cancel-job-id id pool true)))

(defn stop-s
  ([id] (stop-s id false true))
  ([id immediate] (stop-s id immediate true))
  ([id immediate reset]
   (when reset
     (swap! patterns dissoc id))
   (cancel-job-id id pool immediate))
  )

(defn scheduled-jobs
  "Returns a set of all current jobs (both scheduled and recurring)
  for the specified pool."
  [pool]
  (let [pool-atom pool
        ;pool-atom (:pool-atom pool)
        jobs     @(:jobs-ref @pool-atom)
        jobs     (vals jobs)]
    jobs))

(defn- format-start-time
  [date]
  (if (< date (now2))
    ""
    (str ", starts at: " (format-date date))))

(defn- recurring-job-string
  [job]
  (str "[" (:id job) "]"
       ", period: " (:ms-period job) "ms"))


(defn- job-string
  [job]
  (cond
   (= RecurringJob (type job)) (recurring-job-string job)))

(defn show-schedule2
  "Pretty print all of the pool's scheduled jobs"
  ([pool]
     (let [jobs (scheduled-jobs pool)]
       (if (empty? jobs)
         (println "No jobs are currently scheduled.")
         (dorun
          (map #(println (job-string %)) jobs))))))


(defn now2
  "Return the current time in ms"
  []
  (System/currentTimeMillis))
