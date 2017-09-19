(ns techno.player
  (:use [overtone.core :exclude [now stop show-schedule]]
        [techno.synths]
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
(defonce listeners (atom {}))
(declare mk-pool)
(declare play)
(declare mod-p)
(declare get-state)
(declare active?)
(declare p-size)
(declare scheduled-jobs)
(declare set-size)
(declare seq-to-p)
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

(defn get-state
  ([id] @(:state (get @(:jobs-ref @pool) id)))
  ([id key]
   (get @(:state (get @(:jobs-ref @pool) id)) key)
   )
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
         job (schedule-job @pool
                           play
                           period
                           (merge {:bpm bpm :size size :div div}
                                  args)
                           (ref 1))]
     (:id job)
     ))
  )

(defn add-listener [player key f]
  (swap! listeners assoc-in [player key] f)
  )

(defn p-size [p & [s-div]]
  (let [b (apply max (filter number? (keys p)))
        step (if (not (nil? s-div)) (/ s-div (get p :div)) 1)
        s (* (get p :div) (dec b) step)
        note (if (not (empty? (get p b)))
               (apply max (keys (get p b)))
               1)
        s (+ s (* step note))]
    s
    )
  )

(defn- update-player [id]
  (try
    (when (not (empty? (get @patterns id)))
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
                                   ;; x (println k " bar-size " s " note-size " (* step
                                   ;;           (if (not (empty? (get p b)))
                                   ;;             (apply max (keys (get p b)))
                                   ;;             1)))
                                   s (+ s (* step
                                             (if (not (empty? (get p b)))
                                               (apply max (keys (get p b)))
                                               1)))]
                               (swap! patterns assoc-in [id k :size] s)
                               s
                               ))
                           (get @patterns id)))]
                                        ;(println div (:div @state))
          (when (or (not (= 0 (mod div (:div @state)))) (> div (:div @state)))
            (stop-s id true false)
            (get-s bpm {:id id :div div :size size}))
          (set-size id size)
          ))
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
                   p-fx (techno.sequencer/get-pattern-fx k)]
               ;; (println "k " k "bar " bar "note " note)
               (doseq [[a args] (partition 2 (get-in v [bar note]))]
                 (let [args (if (not (nil? (:bus p-fx))) (concat args [:out-bus (:bus p-fx)]) args)
                       args (if (not (nil? (:group p-fx))) (concat [[:head (:group p-fx)]] args) args)]
                   (when (not (nil? a)) (apply a args)))
                 ))
             )
           )

         )
       (doseq [f (vals (get @listeners id))]
         (f beat))
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
;; (def p (get-s 120))
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
;;            (build-map-p [h h h :2 h h h :4 h h h :1] 1/3) :hat))
;; (stop-s p)

(defn add-p
  ([id pattern key] (add-p id pattern key {}))
  ([id pattern key attrs]
   (techno.sequencer/handle-pattern-fx key attrs false)
   (swap! patterns assoc-in [id key] (seq-to-p (merge pattern attrs)))
   (update-player id)
   nil)
  )

(defn rm-p [id key]
  (techno.sequencer/handle-pattern-fx key {} true)
  (if (= key :all)
    (swap! patterns dissoc id)
    (swap! patterns (fn [p] (assoc p id (dissoc (get p id) key)))))
  (update-player id)
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
  (let [step (:step @(:state (get-job id)))]
    (stop-s id true false)
    (get-s speed {:id id})
    (update-player id)
   )
  )

(defn active? [id]
  (if (and (not (nil? @pool)) (not (.isShutdown (:thread-pool @pool))))
      (contains? (scheduled-jobs) id))
  )

;; (defn build-rest-p [pattern & [div]]
;;   (let [div (if div (int (/ 1 div)) (:div pattern))
;;         div (if div div 4)
;;         size (p-size (assoc pattern :div div))]
;;     (loop [p [] beat 1 space 0 start? true prev 0]
;;       (let [end? (>= beat size)
;;             action (get-in pattern (get-pos beat div) [])
;;             is-action (and (sequential? action) (not (nil? (first action))))
;;             [x y] (map #(- %1 %2)
;;                        (get-pos beat div)
;;                        (get-pos (if start? beat prev) div))
;;             start? (and start? (not is-action))
;;             ;n (if (or start? end?) space (dec space))
;;             sep (if (or (> x 0) (> y 1))
;;                   (vec (concat (repeat x :|) (if (> y 1) [(keyword (str y))] []))) [])
;;             p (if (or is-action end?)
;;                 (vec (concat p sep
;;                              ;(if (> n 0) [(keyword (str n))] [])
;;                              [action]
;;                              ))
;;                 p)
;;             space (if is-action 0 (inc space))
;;             prev (if is-action beat prev)]
;;         (if end?
;;           p
;;           (recur p (inc beat) space start? prev)
;;           )))
;;     )
;;   )

(defn build-rest-p [pattern & [div]]
  (let [div (if div (int (/ 1 div)) (:div pattern))
        div (if div div 4)
        size (p-size (assoc pattern :div div))
        is-action? #(and (sequential? %) (not (nil? (first %))))]
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
(defn is-phrase? [pattern]
  (let [size (p-size pattern)
        pattern (into {} (mapcat
                          (fn [b]
                            [(vector b (get-in pattern (get-pos b (:div pattern)) []))])
                          (range 1 (inc size))))]
    (techno.sequencer/is-phrase? pattern))
  )



(defn build-map-p [pattern & [div]]
  (let [div (if div (int (/ 1 div)) 4)
        n-div #(int (* (Math/ceil (/ % div)) div))
        is-space? #(and (keyword? %) (re-find #"^\d" (name %)))]
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
            p (cond  (sequential? a) (assoc-in p pos a)
                     (and end? (or (and (is-space? a)
                                        (> (-> a name Integer/parseInt) 0))
                                   (and (not (= 0 (mod prev-b div))) (= :| a)) ))
                     (assoc-in p (get-pos beat div) [])
                     true p)]
        (if end?
          p
          (recur p pattern beat false))
        )
      )
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

;; (defn phrase-p [inst pattern div & [space args mk-note]]
;;   (let [note-arg (if (or (instance? overtone.studio.inst.Inst inst)
;;                          (instance? overtone.sc.synth.Synth inst))
;;                    (cond (some #(= (:name %) "freq") (:params inst)) :freq
;;                          (some #(= (:name %) "note") (:params inst)) :note
;;                          true false))
;;         note-p #(do ;%
;;                   (if (= note-arg :freq) (midi->hz (note %)) (note %))
;;                   )
;;         mk-note (if mk-note mk-note (fn [n & [n-args]] (vector inst (vec (concat [note-arg (note-p n)] (if n-args n-args args))))))
;;         is-space? #(and (keyword? %) (re-find #"^\d" (name %)))
;;         is-n? #(and (keyword? %) (re-find #"^\w" (name %)))
;;         is-note? #(or is-n? (and (sequential? %) (is-n? (first %))))
;;         is-arg? #(and (sequential? %) (or (number? (first %)) (number? (second %)) (empty? %)))
;;         space (if (and (number? space) (> space 0)) space nil)]
;;     (build-map-p (mapcat (fn [a b]
;;                            (let [a (cond (is-arg? a) nil
;;                                          (sequential? a)
;;                                          (vec (mapcat #(cond (is-arg? (second %)) (mk-note (first %) (second %))
;;                                                              (is-note? (second %)) (vec (concat (mk-note (first %)) (mk-note (second %))))
;;                                                              true (mk-note (first %)))
;;                                                       (partition 2 2 [args] a)))
;;                                          (is-space? a) a
;;                                          (is-note? a) (mk-note a (if (is-arg? b) b))
;;                                          true a)
;;                                  a (if (nil? a) [] [a])
;;                                  a (if (do (println b)
;;                                            (and (not (is-space? (first a)))
;;                                                 (not (is-space? b))
;;                                                 space))
;;                                      (conj a (keyword (str space)))
;;                                      a)
;;                                  ]
;;                              a)
;;                            )
;;                          pattern
;;                          (conj (vec (rest pattern)) :end)
;;                          ) div))
;;   )

(defn phrase-p [inst pattern div & [space args mk-note ret-seq is-note?]]
  (let [note-arg (if (or (instance? overtone.studio.inst.Inst inst)
                         (instance? overtone.sc.synth.Synth inst))
                   (cond (some #(= (:name %) "freq") (:params inst)) :freq
                         (some #(= (:name %) "note") (:params inst)) :note
                         true false))
        note-p #(do ;%
                  (if (= note-arg :freq) (midi->hz (note %)) (note %))
                  )
        mk-note (if mk-note mk-note (fn [n & [n-args]] (vector inst (vec (concat [note-arg (note-p n)] (if n-args n-args args))))))
        is-space? #(and (keyword? %) (re-find #"^\d" (name %)))
        is-arg? #(and (sequential? %)  (or (empty? %) (and (or (number? (first %)) (number? (second %))) (or (keyword? (first %)) (keyword? (second %))))))
        is-n? #(or (number? %) (and (keyword? %) (not (nil? (re-find #"^[a-zA-z]" (name %))))))
        is-note? (if is-note? is-note? #(or (is-n? %) (and (sequential? %) (not (is-arg? %)) (is-n? (first %)))))
        space (if (and (number? space) (> space 0)) (keyword (str space)) nil)
        merge-args (fn [a & [b]]
                     (vec (flatten (into [] (apply hash-map (concat a b))))))
        mk-action (fn [a b]
                    (cond (and (is-note? a) (sequential? a))
                          [(vec (apply concat (phrase-p inst a div 0 args mk-note true is-note?)))]
                          (is-note? a) [(mk-note a
                                                 (merge-args args (if (is-arg? b) b []))
                                                 ;(if (is-arg? b) b args)
                                                 note-arg)]
                          (is-arg? a) nil
                          true [a]))
                                        ;x (println pattern (conj (vec (rest pattern)) nil))
        pattern (if (map? pattern)
                  (let [pattern (assoc pattern :div (int (/ 1 div)))]
                      (reduce
                       (fn [p b]
                         (let [pos (get-pos b (int (/ 1 div)) (p-size pattern))
                               action (get-in pattern pos [])
                               action
                               (do
                                 (cond (and (sequential? action)
                                            (not (empty? action)))
                                       (vec (apply concat (phrase-p inst action div nil
                                                             args mk-note true is-note?)))
                                       (or (keyword? action) (not (empty? action))) (vec (mk-note action args note-arg))))
                               ]
                           (if (or (= b (p-size pattern)) (not (nil? action)))
                             (assoc-in p pos action)
                             p)
                           )
                         )
                       pattern
                       (range 1 (inc (p-size pattern)))))
                  (mapcat (fn [a b]
                            (let [;x (println a b  (and (is-note? a) (is-note? b)))
                                  res (mk-action a b)
                                  res (if (and (is-note? a) (is-note? b) space) (conj res space) res)]
                              res
                              ))
                          pattern
                          (conj (vec (rest pattern)) nil)))
        pattern (if (or ret-seq (map? pattern))
                  pattern
                  (build-map-p
                   pattern
                   div))]
    pattern
    )
  )

(defn scale-p [inst n-note type notes div & [space args]]
  (let [pitches (scale (keyword n-note) (keyword type))
        note-fn (fn [p & [s-args note-arg]]
                  (let [[s n modify oct]
                        (first (re-seq
                                #"([1-9])([b#><]+)*\|?([1-9]+)?"
                                (str p)))
                        n (nth pitches
                               (dec
                                (Integer/parseInt n)))
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
                        n (if (= note-arg :freq) (midi->hz n)
                              n)]
                    (vector inst
                            (vec (concat [note-arg n]
                                         (if s-args s-args args))))))]
    (phrase-p inst notes div nil args note-fn
              false
              #(do
                   (if (keyword? %) (re-matches #"([1-9])([b#><]+)*\|?([1-9]+)?" (name %))
                       false)))
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
     ;(swap! patterns dissoc id)
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
