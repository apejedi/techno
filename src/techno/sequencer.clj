(ns techno.sequencer
  "Functions to execute osc messages/synths"
  (:use [overtone.core]
        [techno.scheduler]
        [techno.helper])
  )

(defonce ^:private patterns (atom {})) ;State containing all patterns
(declare play)
(declare mod-p)
(declare add-p)
(declare rm-p)
(declare update-player)


(defn get-seq [bpm & [options]]
  "Create a sequencer instance which is essentially a scheduled job in the pool"
  (let [div (get options :div 4)
        period (calc-period bpm div)
        options {:bpm bpm :div div}]
    (techno.scheduler/start-sched)
    (techno.scheduler/start-job period play options))
  )

(defn stop-seq [id]
  "Stops a given sequencer"
  (techno.scheduler/stop-sched id))


  
(defn play [id]
  "Main event handler which executes actions when a sequencer instance is running"
  (let [state (get-state id)
        counter (:counter state)
        beat @(:counter state)
        seq-size (get state :size 4)
        seq-div (get state :div 4)]
    ;(println "Beat " beat " seq-size " seq-size)
    (doseq [[name data] (get @patterns id)]
      (let [div (:div data) 
            [bar note] (get-pos beat seq-size seq-div)
            actions (cond (fn? (:fn data)) ((:fn data) patterns [id name] bar note)
                          (fn? (get data bar)) ((get data bar) patterns [id name] note)
                          (fn? (get-in data [bar note])) ((get-in data [bar note]) patterns [id data])
                          true (get-in data [bar note]))]
        (doseq [[a args] (partition 2 actions)]
          (apply a args)           
          )
        ))
    (dosync
     (if (>= beat seq-size)
       (ref-set counter 1)
       (commute counter inc)))
    )
  )

(defn- mod-p [& args]
  "Modify pattern state"
  (let [val (last args)
        path (butlast args)]
      (swap! patterns assoc-in path val)
    nil))


(defn add-p
  "Add a pattern to a sequencer [sequencer-id pattern-data pattern-name]"
  ([id pattern key] (add-p id pattern key {}))
  ([id pattern key attrs]
   (swap! patterns assoc-in [id key] (merge pattern attrs))
   (update-player id)
   nil)
  )

(defn rm-p [id key]
  "Remove a pattern from a sequencer [sequencer-id pattern-name]"
  (if (= key :all)
    (swap! patterns dissoc id)
    (let [pat (get-in @patterns [id key])]
      (swap! patterns (fn [p] (assoc p id (dissoc (get p id) key))))
      )
    )
  (update-player id)
  )


(defn- update-player [id]
  "Function to update the sequencer state whenever a pattern is added or removed"
  (try
    (when (not (empty? (get @patterns id)))
      (let [state (get-state id)
            divs (conj (map #(get % :div) (vals (get @patterns id))) (:div state))
            div (apply lcmv divs)
            bpm (:bpm state)
            old-size (:size state)
            size (apply max
                        (map
                         (fn [[k p]]
                           (let [s (p-size p div)]
                             (swap! patterns assoc-in [id k :size] s)
                             s
                             ))
                         (get @patterns id)))
            counter (:counter state)
            new-counter (apply get-beat
                               (conj (get-pos @counter (:div state) size div)
                                     div))]
        (when (or (not (= 0 (mod div (:div state)))) (> div (:div state)))
          (stop-seq id)
          (get-seq bpm (merge state {:id id :div div :size size})))
        (config-job id {:size size :div div})
        (dosync
         (ref-set counter new-counter))
        ))
    (catch Exception e
      (.println System/out (str "caught exception: " (.getMessage e)))
                                        ;(.printStackTrace e)
      )
    )
  )
