(ns techno.pattern
  "Functions to parse patterns"
  [:use
   [overtone.core]
   [techno.helper]]
  )

(defn build-map-p [pattern & [div is-space? bar-fn bar-note-fn]]
  "Converters sequence based pattern into a map"
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
(defn phrase-p [inst pattern div & [space args mk-note ret-seq is-note? is-space? mk-block]]
  "Main DSL parser for building patterns"
  (let [note-arg (if (or (instance? overtone.studio.inst.Inst inst)
                         (instance? overtone.sc.synth.Synth inst)
                         ;(instance? techno.synths.Sc-synth inst)
                         )
                   (cond (some #(= (:name %) "freq") (:params inst)) :freq
                         (some #(= (:name %) "note") (:params inst)) :note
                         (some #(= (:name %) "freq1") (:params inst)) :freq1
                         true false))
        gated (some #(or (= (:name %) "gate") (= (:name %) "t_gate")) (:params inst))
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
        l-args (if (and (sequential? pattern) (map? (last pattern)))
               (merge-args [] (flatten (into [] (last pattern))))
               nil)
        pattern (if (and (sequential? pattern) (map? (last pattern))) (vec (butlast pattern)) pattern)
        action-fn (fn [action]
                    (cond (and (sequential? action)
                               (not (empty? action)))
                          (vec (apply concat (phrase-p inst action div nil
                                                       args mk-note true is-note?)))
                          (or (keyword? action) (number? action) (not (empty? action)))
                                        ;(vec (mk-note action args note-arg))
                          (vec (mk-note action [] note-arg))
                          ))
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
                                        ;(merge-args args (if (is-arg? b) b []))
                                             (merge-args (if l-args l-args [])
                                                         (if (is-arg? b) b []))
                                             note-arg)]
                          (is-arg? a) nil
                          true [a]))
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
                     {})
                   {:args args}
                   ))]
    pattern
    )
  )


