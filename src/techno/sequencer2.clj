(ns techno.sequencer2
  (:import [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit ThreadPoolExecutor]
           [java.io Writer]))

(defrecord PoolInfo [thread-pool jobs-ref id-count-ref])
(defrecord MutablePool [pool-atom])
(defrecord RecurringJob [id ms-period
                         job pool-info
                         scheduled? state])

(defonce pool (atom nil))
(defonce patterns (atom {}))
(declare mk-pool)
(declare schedule-job)
(declare now)
(declare update-size)
(declare handle-beat-trigger)
(declare shutdown-pool-now!)

(defn get-val-if-ref [x]
  (if (instance? clojure.lang.Atom x)
    @x
    x
    )
  )

(defn- get-job [id]
  (get @(:jobs-ref @pool) id)
  )

(defn- to-str [inst]
  (if (and (coll? inst) (contains? inst :name))
    (:name inst)
    inst
    ))

(defn get-s
  ([speed] (get-s speed 1))
  ([speed step & [options]]
   (if (or (nil? @pool) (.isShutdown (:thread-pool @pool)))
     (swap! pool (fn [_] (mk-pool))))
   (let [period (/ 1000 (/ speed step))
         args (if (not (nil? options)) options {})
         job (schedule-job @pool
                           handle-beat-trigger
                           period
                           (merge {:speed speed :step step :beat 1 :size 1}
                          args))]
     (:id job)
     ))
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
    (get-s speed step {:id id})
   )
  )

(defn add-p
  "(add-p sequencer pattern) adds the given pattern to the patterns being played by the sequencer"
  ([sequencer pattern] (add-p sequencer pattern (gensym "pat") false))
  ([sequencer pattern key] (add-p sequencer pattern key false))
  ([sequencer pattern key wrap]
   (let [id sequencer
         cur-val (get @patterns id {})
         watcher-key (keyword (gensym "pattern"))]
     (swap! patterns (fn [p]
                       (assoc p id
                              (assoc cur-val key {:data pattern :wrap wrap}))))
     (update-size sequencer)
     (if (instance? clojure.lang.Atom pattern)
       (do
         (add-watch pattern watcher-key (fn [& args] (update-size sequencer)))
         )
       )
     ))
  )

(defn rm-p [sequencer pattern]
    "(rm-p sequencer pattern) stops the given pattern from being played"
  (let [id sequencer
        cur-val (get @patterns id {})]
    (swap! patterns assoc id
           (if (keyword? pattern)
             (dissoc cur-val pattern)
             (into {}
                   (remove
                    (fn [[k v]] (= (get-val-if-ref pattern) (get-val-if-ref (v :data))))
                    cur-val
                    )))

           )
    (update-size sequencer)
    )
  )
(defn play
  "Function to play instruments on the given beat"
  ([cur-beat pattern] (play cur-beat pattern false))
  ([cur-beat pattern wrap]
   (let [beat-actions
         (cond
           (fn? pattern) (pattern cur-beat)
           (map? pattern) (pattern cur-beat)
           (sequential? pattern)
           (if (<= cur-beat (count pattern)) (nth pattern (dec cur-beat))))]
     ;(println (System/currentTimeMillis))
     ;; (println cur-beat)
     ;; (if (> (count beat-actions) 0)
     ;;   (println "playing "
     ;;            (reduce (fn [a b] (str (to-str a) " " (to-str b) " ")) beat-actions)
     ;;            " for beat " cur-beat))
     (dorun
      (for [[instrument args] (partition 2 beat-actions)]
        (if (not (nil? instrument))
          (let [inst (apply instrument args)]
            (if (and (instance? overtone.studio.inst.Inst instrument) ;;If instrument has a gate argument, set it to 0 after trigger
                     (some #(= (:name %) "gate") (:params instrument)))
              (do
                ;; (at (+ (now)
                ;;        (* (if (>= (.indexOf args :dur) 0)
                ;;             (nth args (inc (.indexOf args :dur)))
                ;;             1
                ;;             ) 1000))
                ;;     (ctl inst :gate 0)
                ;;     )
                )
              )
            ))))))
  )

(defn- handle-beat-trigger [id]
  (let [job (get-job id)
        job-state @(:state job)
        size (:size job-state)
        step (:step job-state)
        stepped-size (inc (/ (dec size) step))
        beat (cond
               (> (:beat job-state) stepped-size) 1
               true (:beat job-state))
        step-beat (fn [beat step]
                    (let [res (inc (* (dec beat) step))]
                      (cond (=  (mod res (int res)) 0.0)
                           (int res)
                           true res)))
        stepped-beat (step-beat beat step)]
    ;(println "playing " stepped-beat)
    (doseq [[k p] (get @patterns id)]
      (let [val (get-val-if-ref (p :data))
            wrap (p :wrap)
            size (cond
                   (map? val) (apply max (keys val))
                   (sequential? val) (count val)
                   true 1)
            wrap-beat (fn [beat size]
                        (let [s (+ 1 (/ (- size 1) step))
                              steps (cycle (range 1 (inc s)))
                              w (nth steps (dec beat))]
                          (step-beat w step)
                          )
                        )
            final-beat (if (and wrap (> stepped-beat size))
                         (wrap-beat beat size)
                         stepped-beat)]
        (play final-beat val)
        ))
    (dosync
     (swap! (:state job) assoc :beat (inc beat)))
    )
  )
(defn- update-size [id]
  (let [sequencer (get-job id)]
    (swap! (:state sequencer) assoc :size
           (reduce (fn [c p]
                     (let [val (get-val-if-ref (p :data))]
                       (max c
                            (cond
                              (map? val)  (apply max (keys val))
                              (sequential? val) (count val)
                              true (get-in sequencer [:state :size])))
                       ))
                   1
                   (vals (get @patterns (:id sequencer)))
                   )))
  )

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
  [pool-info fun ms-period job-args]
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
                                        :state (atom job-args)})]
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

(defn stop
  "Stop a recurring or scheduled job gracefully either using a
  corresponding record or unique id. If you specify an id, you also
  need to pass the associated pool."
  ([job] (cancel-job job false))
  ([id pool] (cancel-job-id id pool false)))

(defn kill
  "kill a recurring or scheduled job forcefully either using a
  corresponding record or unique id. If you specify an id, you also
  need to pass the associated pool."
  ([job] (cancel-job job true))
  ([id pool] (cancel-job-id id pool true)))

(defn stop-s
  ([id] (stop-s id false))
  ([id immediate]
   (swap! patterns dissoc id)
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
  (if (< date (now))
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

(defn show-schedule
  "Pretty print all of the pool's scheduled jobs"
  ([pool]
     (let [jobs (scheduled-jobs pool)]
       (if (empty? jobs)
         (println "No jobs are currently scheduled.")
         (dorun
          (map #(println (job-string %)) jobs))))))


(defn now
  "Return the current time in ms"
  []
  (System/currentTimeMillis))

(defn- mod-p [sequencer pattern attr val]
  (swap! patterns (fn [p]
                     (let [id sequencer
                           [key pat] (first
                                      (filter (fn [[k v]]
                                                (= (v :data)  pattern)) (p id)))]
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
