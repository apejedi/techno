(ns techno.player
  (:import [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit ThreadPoolExecutor]
           ))

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
   (let [period (/ 60000 bpm 4)
         args (if (not (nil? options)) options {})
         job (schedule-job @pool
                           play
                           period
                           (merge {:bpm bpm :beat 1 :size 1 :div 4}
                          args))]
     (:id job)
     ))
  )

(defn- update-player [id]
  (let [divs (map #(get % :div) (get @patterns id))
        div (apply lcmv divs)]
    )
  )


(defn play [id]
  (let [state (:state (get @(:jobs-ref @pool) id))
        beat (:beat @state)
        size (:size @state)
        s-div (:div @state)]
    (doseq [[k v] (get @patterns id)
            div (get v :div)
            step (/ s-div div)]
      (when (or (= 1 beat) (= 0 (mod beat step)))
        (let [bar (/ beat div)
              note (mod beat div)]
          (doseq [[a args] (partition 2 (get-in v [bar note]))]
              (apply a args)
              ))))
    (if (> beat size)
      (swap! state assoc :beat 1)
      (swap! state assoc :beat (inc beat))))
  )

(defn add-p [id pattern key]
  (update-player)
  (swap! patterns assoc-in [id key] pattern)
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
