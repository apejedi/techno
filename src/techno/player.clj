(ns techno.player
  (:use [overtone.core :exclude [now stop show-schedule]]
        [techno.synths]
        )
  (:import [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit ThreadPoolExecutor]
           [java.io Writer]
           [java.nio.channels AsynchronousSocketChannel]
           [java.net InetSocketAddress]
           [java.nio ByteBuffer]))

(defrecord PoolInfo [thread-pool jobs-ref id-count-ref])
(defrecord MutablePool [pool-atom])
(defrecord RecurringJob [id ms-period
                         job pool-info
                         scheduled? state counter])

(defonce pool (atom nil))
(defonce patterns (atom {}))
(defonce listeners (atom {}))
(defonce buffers (atom {}))
(defonce tekno-client (atom nil))
(defonce tekno-client-connected (atom nil))
(defonce tekno-buffer (atom (ByteBuffer/allocate 12)))
(defonce send-offsets (atom true))
(defonce written (atom true))
(declare mk-pool)
(declare play)
(declare mod-p)
(declare get-state)
(declare active?)
(declare p-size)
(declare scheduled-jobs)
(declare set-size)
(declare rm-p)
(declare seq-to-p)
(declare stop-s)
(declare schedule-job)
(declare now2)
(declare update-size)
(declare handle-beat-trigger)
(declare shutdown-pool-now!)
(defonce node-statuses (atom {}))

(on-event "/n_end"
          (fn [info]
            (swap! node-statuses assoc-in [techno.core/player (first (:args info))] 0))
          :node-freed)
(defn get-val-if-ref [x]
  (if (instance? clojure.lang.Atom x)
    @x
    x
    )
  )
(defn sc-node-info [node]
  (let [ res (recv "/n_info")
        s (snd "/n_query" (to-sc-id node))]
    (deref res 100 false)))

(defn get-pos [beat div & [size s-div]]
  (let [step (if s-div (/ s-div div) div)
        beat (cond (= size 1) 1
                   (and size (> beat size))
                   (if (= (mod beat size) 0)
                     (min size div)
                     (mod beat size))
                   true beat)
        ret-beat (if s-div (or (= 1 beat) (= 0 (mod (dec beat) step))) true)
        bar (cond s-div (inc (int (/ (dec beat) s-div)))
              (= 0 (mod beat div)) (/ beat div)
              true (inc (int (/ beat div))))
        n (if s-div
            (inc (int (/ (dec (mod beat s-div)) step)))
            (mod beat div))
        n (if (= 0 n) div n)]
    (if ret-beat
      [(int bar) (int n)]
      [0 0])
    ))

(defn get-beat [bar note div]
  (+ (* (dec bar) div) note)
  )

(defn mk-tekno-client []
  (let [addr (InetSocketAddress. "127.0.0.1" 10000)
        client (AsynchronousSocketChannel/open)
        connect (try
                  (.connect client addr)
                  (catch Exception e
                    (println (.getMessage e))))]
    (reset! tekno-client client)
    (reset! tekno-client-connected true)
    client
    )
  )

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

(defn get-state [id & [key]]
  (let [state (:state (get @(:jobs-ref @pool) id))]
    (if (not (nil? state))
      (if key
        (get @state key)
        @state)))
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
         period (/ 60000000 bpm div)
         args (if (not (nil? options)) options {})
         counter (get args :counter (ref 1))
         job (schedule-job @pool
                           play
                           period
                           (merge {:bpm bpm :size size :div div :counter counter}
                                  args)
                           counter)
         ]
     (:id job)
     ))
  )



(defn add-listener [player key f]
  (swap! listeners assoc-in [player key] f)
  )

(defn p-size [p & [s-div ret-pos]]
  (let [b (if (contains? p :p-size) (first (:p-size p))
              (apply max (filter number? (keys p))))
        note (if (contains? p :p-size)
               (second (:p-size p))
               (if (not (empty? (get p b)))
                 (apply max (keys (get p b)))
                 1))
        step (if (not (nil? s-div)) (/ s-div (get p :div)) 1)
        div (if s-div s-div (:div p))
        s (* (get p :div) (dec b) step)
        s (+ s (* step note))]
    (if ret-pos
      (get-pos s div)
        s)
    )
  )

(defn- update-player [id]
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
            (stop-s id true false)
            (get-s bpm (merge state {:id id :div div :size size})))
          (set-size id size)
          (dosync
           (ref-set counter new-counter))
          ))
    (catch Exception e
      (.println System/out (str "caught exception: " (.getMessage e)))
      ;(.printStackTrace e)
      )
    )
  )


(defn play [id]
  (try
    (when (not (nil? (get-state id)))
        (let [state (get-state id)
              counter (:counter (get @(:jobs-ref @pool) id))
              beat @counter
              size (:size state)
              s-div (:div state)]
          (when (not (get state :queued false))
            (doseq [[k v] (get @patterns id)]
              (let [beat (if (< (:size v) beat) (mod beat (:size v)) beat)
                    beat (if (= beat 0) (:size v) beat)
                    div (get v :div)
                    step (/ s-div div)
                    queued (and (not (= beat 1)) (get v :add-at-1 false))]
                (when (= beat 1)
                  (mod-p id k :add-at-1 false))
                (when (and (not queued) (or (= 1 beat) (= 0 (mod (dec beat) step))))
                  (let [bar (inc (int (/ (dec beat) s-div)))
                        note (inc (int (/ (dec (mod beat s-div)) step)))
                        note (if (= 0 note) div note)
                        p-fx (techno.sequencer/get-pattern-fx k)
                        actions (cond (fn? (:fn v)) ((:fn v) patterns [id k] bar note)
                                      (fn? (get v bar)) ((get v bar) patterns [id k] note)
                                      (fn? (get-in v [bar note])) ((get-in v [bar note]) patterns [id k])
                                      true (get-in v [bar note]))
                        gated (:gated v)
                        synth (:synth v)
                        synth-inst (:synth-inst v)
                        mono (:mono v)]
                    (doseq [[a args] (partition 2 actions)]
                      (when (not (nil? a))
                        (let [has-gate (if gated (not (= -1 (.indexOf args :gate))) false)
                              args (if (and (not (nil? (:bus p-fx))) (nil? synth-inst))
                                     (concat args [(if (and (map? a) (contains? a :params) (first (filter #(.equals "outBus" (:name %)) (:params a))))
                                                     :outBus :out-bus)
                                                   (:bus p-fx)]) args)
                              args (if (and (not (nil? (:group p-fx))) (nil? synth-inst))
                                     (concat [[:head (:group p-fx)]] args) args)
                              pos (str [bar (if (fn? (get v bar)) 1 note)])]
                          (if gated
                            (if mono
                              (if (not (nil? synth-inst))
                                (apply ctl (concat [synth-inst] (if has-gate [] [:gate 1]) args))
                                (swap! patterns
                                       assoc-in [id k :synth-inst]
                                       (apply synth args)))
                              (let [n (if (not (= -1 (.indexOf args :note)))
                                        (nth args (inc (.indexOf args :note)))
                                        (hz->midi (nth args (inc (.indexOf args :freq)))))
                                    c-args (if (vector? (first args)) (vec (rest args)) args)
                                    nodes (get-in @patterns [id k :nodes] {})
                                    node (get nodes n)
                                    destroyed (get @node-statuses id)]
                                (if (and node
                                         (not (= 0 (get destroyed (to-sc-id node)))))
                                  (try
                                    (apply ctl (concat [node] c-args))
                                    (catch Exception e))
                                  (do
                                    ;; (if node
                                    ;;   (swap! node-statuses assoc id
                                    ;;          (dissoc destroyed (to-sc-id node))))
                                    (swap! patterns assoc-in [id k :nodes]
                                           (assoc nodes n (apply a args)))))))
                            (apply a args))
                          (when (and (= @send-offsets k) (or (= true @written) (.isDone @written)))
                            (.clear @tekno-buffer)
                            (.put @tekno-buffer (.getBytes pos))
                            (.limit @tekno-buffer (.length pos))
                            (.rewind @tekno-buffer)
                            (reset! written (.write @tekno-client @tekno-buffer)))
                          ))
                      ))
                  )
                )
              )
            (doseq [f (vals (get @listeners id))]
              (f beat))
            (dosync
             (if (>= beat size)
               (ref-set counter 1)
               (commute counter inc))))
          ))
    (catch Exception e (println (.getMessage e))
           (.printStackTrace e)
           (if (= id 0) (stop-s 0))))
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

(defn add-p
  ([id pattern key] (add-p id pattern key {}))
  ([id pattern key attrs]
   (when (:gated pattern)
     (rm-p id key)
     (if-let [mixer (get (techno.sequencer/get-pattern-fx key) :mixer)]
       (ctl mixer :volume 0)
       ))
   (if (:add-at-1 attrs)
     (add-listener
      id key (fn [b]
               (when (= b (get-state id :size))
                 (swap! listeners (fn [l] (update-in l [id] dissoc key)))
                 (add-p id pattern key (dissoc attrs :add-at-1)))
               ))
     (do
       (techno.sequencer/handle-pattern-fx key attrs false (if (map? pattern) (:fx pattern)))
       (swap! patterns assoc-in [id key] (seq-to-p (merge pattern attrs)))
       (update-player id)))
   nil)
  )

(defn rm-p [id key]
  (techno.sequencer/handle-pattern-fx key {} true)
  (if (= key :all)
    (swap! patterns dissoc id)
    (let [pat (get-in @patterns [id key])]
      (swap! patterns (fn [p] (assoc p id (dissoc (get p id) key))))
      (if (:gated pat)
        (doseq [syn (:nodes pat)]
          (try
            (ctl syn :gate 0)
            (catch Exception e)))))
    )
  (update-player id)
  )

(defn play-p [& args]
  (let [tempo (if (number? (last args)) (last args) 80)
        patterns (take-while #(map? %) args)
        x (stop-s 0)
        player (get-s tempo {:id 0 :queued true})]
    (doseq [p patterns]
      (add-p player p (if (contains? p :key) (:key p) (keyword (gensym "pattern"))) {:no-group true}))
    (let [state (get-state player)]
      (add-listener 0 :one-shot
                    (fn [b] (when (= b (:size state))
                              (stop-s 0))))
      (swap! (:state (get @(:jobs-ref @pool) player)) dissoc :queued))
    )
  )
(defn mod-p [& args]
  (let [val (last args)
        path (butlast args)]
      (swap! patterns assoc-in path val)
    nil))

(defn mod-amp [sequencer pattern delta]
  (techno.sequencer/mod-amp nil pattern delta)
  )

(defn set-size [id size]
  (swap! (:state (get-job id)) assoc :size size)
  )

(defn set-st [id step]
  (swap! (:state (get-job id)) assoc :step step)
  )
(defn set-sp [id speed]
  (let [step (:step @(:state (get-job id)))
        counter (:counter (get-job id))
        div (:div (get-state id))]
    (stop-s id true false)
    (get-s speed {:id id :counter counter :div div})
    (update-player id)
   )
  )

(defn active? [id]
  (if (and (not (nil? @pool)) (not (.isShutdown (:thread-pool @pool))))
      (contains? (scheduled-jobs) id))
  )


(defn build-rest-p [pattern & [div]]
  (let [div (if div (int (/ 1 div)) (:div pattern))
        div (if div div 4)
        size (p-size (assoc pattern :div div))
                                        ;is-action? #(and (sequential? %) (not (nil? (first %))))
        is-action? #(and (not (= [] %)) (not (nil? %)))
        ]
    (loop [p [] cur 1 prev 1]
      (let [action (get-in pattern (get-pos cur div))
            is-action (is-action? action)
            end? (>= cur size)
            p (if (or is-action end?)
                (let [pos1 (get-pos prev div)
                      pos2 (get-pos cur div)
                      space (cond (and (not is-action) end?
                                       (not (= (first pos1) (first pos2))))
                                  (second pos2)
                                  (and (not is-action) end?) (dec (second pos2))
                                  (= (first pos1) (first pos2))
                                  (dec (- (second pos2) (second pos1)))
                                  true (dec (second pos2)))
                      space (if (> space 0) [(keyword (str space))])
                      sep (concat (repeat (- (first pos2) (first pos1)) :|)
                                  space)]
                  (vec (concat p sep (if is-action [action])))
                  )
                p)
            prev (if is-action cur prev)]
        (if end?
          p
          (recur p (inc cur) prev)
          )))
    )
  )

(defn build-phrase-p [pattern]
  (let [action (first (filter #(not (nil? (first %))) (map (fn [b] (get-in pattern (get-pos b (:div pattern)))) (range 1 (inc (p-size pattern))))) )
        inst (first action)
        params (vec (flatten (into [] (filter (fn [[k v]] (and (not (= k :note)) (not (= k :freq)))) (partition 2 (second action))))))
        rest-p (build-rest-p pattern)
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
    [inst phrase-p (/ 1 (:div pattern)) params]
    )
  )


(defn step-mode-p [pattern div rest-regex]
  (let []
    (loop [cur (first pattern) pos 1 pattern pattern new-pattern [] prev (first pattern)]
      (let [is-rest (or (= :| cur) (and (keyword? cur) (re-matches (re-pattern rest-regex) (name cur))))
            old-pos pos
            [r re] (if (.contains rest-regex "0") [:01 "0"] [:1 ""])
            [res cur pattern pos prev] (if is-rest
                                         (if (= :| cur)
                                           (do
                                             (if (and (not (= :| prev)) (= 0 (mod (dec pos) div)))
                                               [[] (second pattern) (rest pattern) pos cur]
                                               (let [[bar note] (get-pos pos div)
                                                     rests (- div (dec note))]
                                                 [(into (vec (repeat rests r)) [:|]) (second pattern) (rest pattern) (+ pos rests) cur])))
                                           (let [n (Integer/parseInt (name cur))]
                                             (if (> n 0) [[r] (keyword (str re (dec n))) pattern (inc pos) cur]
                                                 [[] (second pattern) (rest pattern) pos cur])))
                                    [[cur] (second pattern) (rest pattern) (inc pos) cur])
            res (if (and (= div (second (get-pos old-pos div))) (not (or (= :00 prev) (= :0 prev))) (not (= :| prev)))
                  (conj res :|)
                  res)
   ;         x (println prev cur res old-pos)
            new-pattern (into new-pattern res)]
        (if (> (count pattern) 0)
          (recur cur pos pattern new-pattern prev)
          new-pattern)
        )
      )
    )
  )


(defn text-mode-p [pattern div rest-regex]
  (let [pattern (step-mode-p pattern div rest-regex)]
      (reduce
       (fn [p a]
         (let [l (last p)]
           (into (vec (butlast p))
                 (cond (and (keyword l) (re-matches (re-pattern rest-regex) (name l)) (keyword a) (re-matches (re-pattern rest-regex) (name a)))
                       [(keyword (str "0" (+ (Integer/parseInt (name l)) (Integer/parseInt (name a)))))]
                       (and (= :| a) (keyword l) (re-matches (re-pattern rest-regex) (name l))) [a]
                   true [l a]))))
       [(first pattern)]
       (rest pattern)
       ))
  )

(defn shift-step-p [pattern div pos dir rest-regex]
  (let [pattern (step-mode-p pattern div rest-regex)
        pattern (vec (filter #(not (= :| %)) pattern))
        pos (dec (if (number? pos) pos (get-beat (first pos) (second pos) div)))]
    (if (= dir :right)
      (into (into (subvec pattern 0 pos) [:01]) (subvec pattern pos))
      (into (vec (butlast (subvec pattern 0 pos))) (into (subvec pattern pos) [:01]))
      )
    )
  )

(defn move-step-p [pattern div pos dir rest-regex]
  (let [pattern (step-mode-p pattern div rest-regex)
        pattern (vec (filter #(not (= :| %)) pattern))
        pos (dec (if (number? pos) pos (get-beat (first pos) (second pos) div)))]
    (if (= dir :right)
      (assoc (assoc pattern (inc pos) (nth pattern pos)) pos :01)
      (assoc (assoc pattern (dec pos) (nth pattern pos)) pos :01)
      )
    )
  )

(defn is-phrase? [pattern]
  (let [size (p-size pattern)
        pattern (into {} (mapcat
                          (fn [b]
                            [(vector b (get-in pattern (get-pos b (:div pattern)) []))])
                          (range 1 (inc size))))]
    (techno.sequencer/is-phrase? pattern))
  )



(defn build-map-p [pattern & [div is-space? bar-fn bar-note-fn]]
  (let [div (if div (int (/ 1 div)) 4)
        n-div #(int (* (Math/ceil (/ % div)) div))
        is-space? (if is-space? is-space? #(and (keyword? %) (re-find #"^\d" (name %))))]
    (loop [p {:div div} pattern pattern beat 1 start true]
      (let [a (first pattern)
            pos (get-pos beat div)
            pattern (rest pattern)
            end? (empty? pattern)
            prev-b beat
            beat (if (= :| a)
                   (cond (= 0 (mod beat div))
                         (cond (or end? (is-space? (first pattern))) beat
                               true (inc beat))
                         (or (is-space? (first pattern)) end?) (n-div (inc beat))
                         true (inc (n-div (inc beat))))
                   (+ beat
                      (cond
                        (keyword? (first pattern)) 0
                        (keyword? a) (if (or start end?) (-> a name Integer/parseInt)
                                         (inc (-> a name Integer/parseInt)))
                        true 1)))
            p (cond
                (fn? a) (cond (and (= 1 (second pos)) (= :| (first pattern)))
                              (assoc p (first pos)
                                     (if bar-fn (bar-fn a (first pos)) a))
                              true
                              (assoc-in p pos
                                        (if bar-note-fn (bar-note-fn a (first pos) (second pos))
                                            a)))
                (or (string? a) (sequential? a)) (assoc-in p pos a)
                (and end? (or (and (is-space? a)
                                   (> (-> a name Integer/parseInt) 0))
                              (and (not (= 0 (mod prev-b div))) (= :| a)) ))
                (if (fn? (get p (first (get-pos beat div))))
                  (assoc p :p-size (get-pos beat div))
                  (assoc-in p (get-pos beat div) []))
                true p)]
        (if end?
          p
          (recur p pattern beat false))
        )
      )
    )
  )

(defn build-phrase-map [pattern note-fn]
  (let [r #(/ (Math/round (* % (Math/pow 10 2))) (Math/pow 10 2))
        mk-args (fn [inst args]
                  (let [names (map #(keyword (:name %)) (:params inst))
                        pos (first (keep-indexed #(if (keyword? %2) %1) args))
                        pos (if (nil? pos) 0 pos)
                        [vals re] (split-at pos args)]
                    (vec (concat (mapcat #(vector %1 %2) names vals) re))
                    ))
        collect-args (fn [args actions]
                       (reduce
                        (fn [m [i a]]
                          (if (and (sequential? a) (> (count a) 0))
                            (reduce
                             #(assoc-in
                               %1 [(:name i)  (first %2) (r (second %2))]
                               (inc (get-in m [(:name i)  (first %2) (r (second %2))] 0)))
                             m
                             (partition 2 (mk-args i a)))
                            m))
                        args
                        (partition 2 actions)))
        args (reduce
              (fn [m o]
                (collect-args m (get-in pattern (get-pos o (:div pattern))))
                )
              {}
              (range 1 (inc (p-size pattern))))]
    args
    )
  )

(defn stretch-p [pattern size]
  (let [o-size (p-size pattern)
        size (if (sequential? size)
               (p-size {:div (:div pattern) (first size) {(second size) []}})
               size)]
    (reduce
     (fn [p b]
       (let [pos (get-pos b (:div p) o-size)
             action (get-in pattern pos [])
             pos2 (get-pos b (:div p))]
         (if (or (= b size) (not (empty? action)))
           (assoc-in p pos2 action)
           p)
         )
       )
     {:div (:div pattern)}
     (range 1 (inc size)))
    )
  )
(defn merge-p [& patterns]
  (let [divs (map #(get % :div) patterns)
        div (apply lcmv divs)
        size (apply max
                          (map
                           (fn [p]
                             (let [b (apply max (filter number? (keys p)))
                                   s (* div (dec b))
                                   step (/ div (get p :div))
                                   s (+ s (* step
                                             (if (not (empty? (get p b)))
                                               (apply max (keys (get p b)))
                                               1)))]
                               s
                               ))
                           patterns))]
    (reduce
     (fn [p b]
       (reduce
        (fn [p c]
          (let [pos (get-pos b (:div c))
                a (get-in c pos)
                o (get-in p pos)]
              (if (and (sequential? a) (not (nil? a)))
                (assoc-in p pos
                          (if (sequential? o)
                            (vec (concat o a))
                            a))
                p))
          )
        p
        patterns)
       )
     {:div div}
     (range 1 (inc size)))
    )
  )

(defn shift-p [pattern dir & [start end]]
  (let [start (if start start 1)
        end (if end end (p-size pattern))
        div (:div pattern)
        size (p-size pattern)
        pattern (reduce
                 (fn [p b]
                   (let [n (if (= dir :left)
                             (dec b)
                             (inc b))]
                     (if (and (> n 0) (<= n size))
                         (assoc-in p (get-pos n div)
                                   (get-in pattern (get-pos b div)))
                         p))
                   )
                 pattern
                 (range start (inc end)))]
    (if (= dir :left)
      (assoc-in pattern (get-pos end div) [])
      (assoc-in pattern (get-pos start div) []))
    )
  )

(defn seq-to-p [pattern]
  (if (not (contains? pattern :div))
    (let [step (techno.sequencer/get-step pattern)
          div (int (/ 1 step))
          size (techno.sequencer/i-step (techno.sequencer/p-size pattern step))
          offsets (map techno.sequencer/i-step (range 1 (+ size step) step))]
      (reduce
       (fn [p o]
         (if (or (= size o) (and (sequential? (get pattern o)) (not (nil? (get pattern o)))))
           (assoc-in p (get-pos (int (+ 1 (/ (- o 1) step))) div) (get pattern o))
           p)
         )
       {:div div 1 {}}
       offsets)
      )
    pattern)
  )


(defn phrase-p [inst pattern div & [space args mk-note ret-seq is-note? is-space? mk-block]]
  (let [note-arg (if (or (instance? overtone.studio.inst.Inst inst)
                         (instance? overtone.sc.synth.Synth inst)
                         (instance? techno.synths.Sc-synth inst))
                   (cond (some #(= (:name %) "freq") (:params inst)) :freq
                         (some #(= (:name %) "note") (:params inst)) :note
                         (some #(= (:name %) "freq1") (:params inst)) :freq1
                         true false))
        gated (some #(= (:name %) "gate") (:params inst))
        note-p #(do ;%
                  (if (= note-arg :freq) (midi->hz (note %)) (note %))
                  )
        mk-note (if mk-note mk-note (fn [n & [n-args]] (vector inst (vec (concat [note-arg (note-p n)] (if n-args n-args args))))))
        is-space? (if is-space? is-space? #(and (keyword? %) (re-find #"^\d" (name %))))
        is-arg? #(and (sequential? %)  (or (empty? %) (and (or (number? (first %)) (number? (second %))) (or (keyword? (first %)) (keyword? (second %))))))
        is-n? #(or (number? %) (and (keyword? %) (not (nil? (re-find #"^[a-zA-z]" (name %))))))
        is-note? (if is-note? is-note? #(or (is-n? %) (and (sequential? %) (not (is-arg? %)) (is-n? (first %)))))
        space (if (and (number? space) (> space 0)) (keyword (str space)) nil)
        merge-args (fn [a & [b]]
                     (vec (flatten (into [] (apply hash-map (concat a b))))))
        args (if (and (sequential? pattern) (map? (last pattern))) (merge-args args (flatten (into [] (last pattern)))) args)
        pattern (if (and (sequential? pattern) (map? (last pattern))) (vec (butlast pattern)) pattern)
        action-fn (fn [action]
                    (cond (and (sequential? action)
                               (not (empty? action)))
                          (vec (apply concat (phrase-p inst action div nil
                                                       args mk-note true is-note?)))
                          (or (keyword? action) (number? action) (not (empty? action))) (vec (mk-note action args note-arg))))
        bar-fn (fn [f bar]
                 (fn [p key b]
                   (let [args (if (some #{3}
                                        (map #(alength (.getParameterTypes %))
                                             (-> f class .getDeclaredMethods)))
                                [p (conj key :data) b]
                                [(get-in @p (conj key :data)) b])
                         a (apply f args)]
                     (swap! p assoc-in (conj key :data bar b) a) (action-fn a))))
        bar-note-fn (fn [f bar note]
                      (fn [p key]
                        (let [args (if (some #{2}
                                        (map #(alength (.getParameterTypes %))
                                             (-> f class .getDeclaredMethods)))
                                [p (conj key :data)]
                                [(get-in @p (conj key :data))])
                              a (apply f args)]
                          (swap! p assoc-in (conj key :data bar note) a)
                          (action-fn a))))
        mk-action (fn [a b]
                    (cond
                      (and (is-note? a) (sequential? a))
                      (if mk-block
                        (mk-block inst a div 0 args mk-note true is-note?)
                        [(vec (apply concat (phrase-p inst a div 0 args mk-note true is-note?)))])
                      (is-note? a) [(mk-note a
                                             (merge-args args (if (is-arg? b) b []))
                                             note-arg)]
                          (is-arg? a) nil
                          true [a]))
                                        ;x (println pattern (conj (vec (rest pattern)) nil))
        pattern (if (map? pattern)
                  (let [pattern (assoc pattern :div (int (/ 1 div)))]
                      (reduce
                       (fn [p b]
                         (let [[bar note] (get-pos b (int (/ 1 div)) (p-size pattern))
                               p (cond (fn? (get pattern bar))
                                       (assoc p bar (bar-fn (get pattern bar) bar))
                                       (fn? (get-in pattern [bar note]))
                                       (assoc-in p [bar note] (bar-note-fn (get-in pattern [bar note]) bar note))
                                       true (if (or (= b (p-size pattern)) (not (nil? (get-in pattern [bar note] []))))
                                              (assoc-in p [bar note] (action-fn (get-in pattern [bar note] [])))
                                              p)
                                        )]
                           p
                           )
                         )
                       pattern
                       (range 1 (inc (p-size pattern)))))
                  (mapcat (fn [a b]
                            (let [res (mk-action a b)
                                  res (if (and (is-note? a) (is-note? b) space)
                                        (conj res space) res)]
                              res
                              ))
                          pattern
                          (conj (vec (rest pattern)) nil)))
        pattern (if (or ret-seq (map? pattern))
                  pattern
                  (merge
                   (build-map-p
                    pattern
                    div is-space? bar-fn bar-note-fn)
                   (if gated
                     {:synth inst :gated true}
                     {})))]
    pattern
    )
  )

(defn scale-p [inst n-note type notes div & [space args]]
  (let [scale (scale (keyword n-note) (keyword type))
        scale (if (= 0 (mod (last scale) (first scale)))
                (butlast scale) scale)
        pitches (cycle scale)
        note-fn (fn [p & [s-args note-arg]]
                  (let [[s no modify oct]
                        (first (re-seq
                                #"([0-9]+)([b#><]+)*\|?([1-9]+)?"
                                (str p)))
                        no (Integer/parseInt no)
                        n (nth pitches
                               (dec
                                no))
                        n (+ n (* 12 (int (/ no (count scale)))))
                        n (if (not (nil? oct))
                            (note
                             (keyword (str
                                       (name (find-pitch-class-name n))
                                       oct)))
                            n)
                        n (if (not (nil? modify))
                            (reduce (fn [n m]
                                      (cond (= \b m) (dec n)
                                            (= \# m) (inc n)
                                            (= \> m) (+ 12 n)
                                            (= \< m) (- n 12)
                                            true n
                                            ))
                                    n modify)
                            n)
                        n (if (or (= note-arg :freq) (= note-arg :freq1)) (midi->hz n)
                              n)]
                    (vector inst
                            (vec (concat [note-arg n]
                                         (if s-args s-args args))))))]
    (phrase-p inst notes div space args note-fn
              false
              #(let [r (fn [n] (re-matches #"([1-9]+)([b#><]+)*\|?([1-9]+)?" n))]
                   (or (and (keyword? %) (r (name %)))
                       (and (sequential? %) (keyword? (first %)) (r (name (first %))))))
              #(and (keyword? %) (= \0 (first (name %)))))
    )
  )





(defn get-p
  [& path]
  (get-in @patterns path)
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
                                      TimeUnit/MICROSECONDS)
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
     (doseq [[k v] (get @patterns id)]
       (rm-p id k))
     (swap! listeners dissoc id)
     (swap! patterns dissoc id)
     (swap! node-statuses dissoc id)
     )
   (cancel-job-id id pool immediate))
  )

(defn scheduled-jobs
  "Returns a set of all current jobs (both scheduled and recurring)
  for the specified pool."
  ([] (scheduled-jobs pool))
  ([pool]
   (if (or (nil? @pool) (.isShutdown (:thread-pool @pool)))
     {}
     (let [pool-atom pool
                                        ;pool-atom (:pool-atom pool)
           jobs     @(:jobs-ref @pool-atom)]
       jobs))))

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

(defn w-choose [actions]
  (let [probs (vals actions)
        actions (keys actions)
        indices (range 0 (count probs))
        total-prob (apply + probs)
        r (* (rand) total-prob)]
    (loop [r (- r (first probs)) probs (rest probs) i 0]
      (if (or (<= r 0) (empty? probs))
        (nth actions i)
        (recur (- r (first probs))
               (rest probs)
               (inc i)))
      )
    )
  )

(defsynth record-p [bus 10 buf 0 run 0]
  (record-buf (in:ar bus 2) buf :action 2 :loop 0 :run run)
  )


(defn record-buf-p [player pattern]
  (let [size (p-size (get-p player pattern))
        div (:div (get-p player pattern))
        dur (+ 1 (* size (/ 60 (get-state player :bpm) div)))
        fx (techno.sequencer/get-pattern-fx pattern)
        buf (buffer (* 44100 dur) 2)
        key (keyword "record-" (name pattern))
        recorder (record-p [:tail (:group fx)] (:bus fx) buf)]
    (if (and (contains? @buffers pattern) (buffer? (get @buffers pattern)))
      (buffer-free (get @buffers pattern)))
    (swap! buffers assoc pattern buf)
    (add-listener
     player key (fn [b]
                  (when (= b 1)
                    (ctl recorder :run 1)
                    (swap! listeners (fn [l] (update-in l [player] dissoc key)))
                    ))
     )
    )
  )
;(record-buf-p techno.core/player :drum1)


(defsynth p-hi-shelf [audio-bus 10 out-bus 0 freq 12000 rs 0.5 db 0]
  (let [source (in:ar audio-bus 2)
        source (b-hi-shelf:ar source freq rs db)]
    (replace-out:ar out-bus source)
    )
  )

(defsynth p-low-shelf [audio-bus 10 out-bus 0 freq 12000 rs 0.5 db 0]
  (let [source (in:ar audio-bus 2)
        source (b-low-shelf:ar source freq rs db)]
    (replace-out:ar out-bus source)
    )
  )

(defsynth p-reverb [audio-bus 10 out-bus 0
                    roomsize 10 revtime 3
                    damping 0.5 inputbw 0.5
                    spread 15 drylevel 1
                    earlyreflevel 0.7 taillevel 0.5]
  (let [source (in:ar audio-bus 1)
        reverb (* 0.3 (g-verb:ar
                  source
                  roomsize
                  revtime
                  damping
                  inputbw
                  spread
                  drylevel
                  earlyreflevel
                  taillevel))]
    (replace-out:ar out-bus reverb
            ;(+ reverb source)
            )
    )
  )

(defsynth p-delay [audio-bus 10 out-bus 0 max-delay 0.2 delay 0.2 decay 1]
  (let [source (in:ar audio-bus 1)
        snd (comb-c:ar source max-delay delay decay)]
    (replace-out:ar out-bus [snd snd])
    )
  )

(defsynth p-compander [audio-bus 10 out-bus 0 thresh 0.5 below 1 above 1 clamp-time 0.01 relax-time 0.1]
  (let [source (in:ar audio-bus 1)
        snd (compander source source thresh  below above clamp-time relax-time)]
    (replace-out:ar out-bus [snd snd])
    )
  )

(defsynth p-pitch-shift [audio-bus 10 out-bus 0 window-size 0.2 pitch-ratio 1 pitch-dispersion 0 time-dispersion 0]
  (let [source (in:ar audio-bus 1)
        snd (pitch-shift source window-size pitch-ratio pitch-dispersion time-dispersion)]
    (replace-out:ar out-bus [snd snd])
    )
  )

(defsynth p-peak-eq [audio-bus 10 out-bus 0 freq 100 db 0 rq 1]
  (let [source (in:ar audio-bus 1)
        snd (b-peak-eq:ar source freq rq db)]
    (replace-out:ar out-bus [snd snd])
    )
  )

(defsynth p-meddis [audio-bus 10 out-bus 0]
  (let [source (in:ar audio-bus 1)
        snd (techno.ugens/meddis source)]
    (replace-out:ar out-bus [snd snd])
    )
  )
