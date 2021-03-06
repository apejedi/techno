(ns techno.drum-patterns
  (:use [techno.drums]
        [techno.sequencer :as s :exclude [t]]
        [techno.synths]
        [overtone.core]
        [overtone.inst.synth]
        [techno.samples]
        [techno.recorder]
        )

  (:require [techno.player :as p]))

(defmacro drum-pattern [kits pattern & [step args]]
  (list 'let [['t1 't2 't3 't4 't5 't6] (list 'map #(str "Tom" %) (list 'range 1 7))
              ['k1 'k2 'k3 'k4 'k5 'k6] (list 'map #(str "Kick" %) (list 'range 1 7))
              ['c1 'c2 'c3 'c4 'c5 'c6] (list 'map #(str "ClHat" %) (list 'range 1 7))
              ['cl1 'cl2 'cl3 'cl4 'cl5 'cl6] (list 'map #(str "Clap" %) (list 'range 1 7))
              ['cy1 'cy2 'cy3 'cy4 'cy5 'cy6] (list 'map #(str "Cymbal" %) (list 'range 1 7))
              ['cr1 'cr2 'cr3 'cr4 'cr5 'cr6 'cr7] (list 'map #(str "Crash" %) (list 'range 1 8))
              ['r1 'r2 'r3 'r4 'r5 'r6] (list 'map #(str "Rim" %) (list 'range 1 7))
              ['ri1 'ri2 'ri3 'ri4 'ri5 'ri6] (list 'map #(str "Ride" %) (list 'range 1 7))
              ['h1 'h2 'h3 'h4 'h5 'h6] (list 'map #(str "HfHat" %) (list 'range 1 7))
              ['f1 'f2 'f3 'f4 'f5 'f6] (list 'map #(str "fx" %) (list 'range 1 7))
              ['o1 'o2 'o3 'o4 'o5 'o6] (list 'map #(str "OpHat" %) (list 'range 1 7))
              ['sd1 'sd2 'sd3 'sd4 'sd5 'sd6 'sd7 'sd8 'sd9 'sd10] (list 'map #(str "SdSt" %) (list 'range 1 11))
              ['s1 's2 's3 's4 's5 's6 's7 's8 's9 's10 's11 's12] (list 'map #(str "Snr" %) (list 'range 1 13))]
        (list 'build-from-kits
              kits
              pattern
              (if step step 0.25)
              (if args args []))
        )
  )

(defmacro drum-patterns [kits & patterns]
  (let [args (last patterns)
        patterns (take-while #(sequential? %) patterns)
        step (first (filter number? patterns))]
    (println patterns)
    `(reduce s/merge-p
            (map (fn [p#] (and (sequential? ~args) ~step)
                           (drum-pattern ~kits p# ~step ~args)
                           (drum-pattern ~kits p# ~step))
                  patterns))
    )
  )

(defn drum-s [kits snd]
  (let [sounds (reduce into [] (map #(drum-kits %) kits))
        s-map {:t "Tom" :k "Kick" :c "ClHat" :cl "Clap"
               :cy "Cymbal" :cr "Crash" :r "Rim" :ri "Ride" :p "Perc"
               :h "HfHat" :f "Fx" :o "OpHat" :sd "SdSt" :s "Snr"}
        lookup (cond (string? snd) snd
                     (keyword? snd) (let [[_ k n] (re-find #"(?i)([a-z]+)(\d+)*" (name snd))]
                                      (str
                                       (if (contains? s-map (keyword k))
                                         (get s-map (keyword k)) (name k))
                                       n)))
        re #"(?i)([a-z]+)[^0-9a-z]*([0-9]+)*"
        re2 #"(?i)([a-z]+)[^0-9a-z]*([0-9]+)*\."]
    (some (fn [s]
            (let [
                  [res name-in n-in] (last (re-seq re lookup))
                  [cur cur-in curn-in] (last (re-seq re2 (name (first s))))
                  name-in (if (nil? name-in) in name-in)
                  cur-in (if (< (count (re-seq re2 (name (first s)))) 2)  (name (first s)) cur-in)]
              (if (and (.contains
                        (.toLowerCase cur-in) (.toLowerCase name-in))
                       (or (nil? curn-in) (nil? n-in)
                           (.contains
                            (.toLowerCase curn-in) (.toLowerCase n-in)))) (last s)))
            )
          sounds)
   )
  )

(defn drum-p2 [kits pattern & [div]]
  (let [mk-action (fn [n & [n-args]]
                    (if (keyword? n)
                      (vector (drum-s kits n)  (if n-args n-args []))
                      [n (if n-args n-args [])]))
        is-drum? #(if (keyword? %) (re-matches #"[a-zA-Z]+[0-9]*" (name %))
                      false)
        div (if div div (:div pattern))
        pattern (if (map? pattern) (assoc pattern :div div) pattern)]
    (if (map? pattern)
      (techno.player/phrase-p nil pattern div nil []
                                mk-action)
        (techno.player/phrase-p nil pattern div nil []
                                mk-action)))
  )

(defn drum-p [kits & patterns]
  (let [args (last patterns)
        step (first (filter number? patterns))
        patterns (take-while #(sequential? %) patterns)
        args (if (and step (sequential? args)) args [])
        step (if step step 0.25)]
    (reduce
     (fn [pattern phrase]
       (s/merge-p
        pattern
        (loop [phrase phrase beat 1 pattern {} prev nil]
          (let [args (vec (if (not (nil? args)) args []))
                sounds (reduce into [] (map #(drum-kits %) kits))
                s-map {:t "Tom" :k "Kick" :c "ClHat" :cl "Clap"
                       :cy "Cymbal" :cr "Crash" :r "Rim" :ri "Ride" :p "Perc"
                       :h "HfHat" :f "Fx" :o "OpHat" :sd "SdSt" :s "Snr"}
                find-snd (fn [in]
                           (some (fn [s]
                                   (let [re #"(?i)([a-z]+)[^0-9a-z]*([0-9]+)"
                                         [res name-in n-in] (last (re-seq re in))
                                         [cur cur-in curn-in] (last (re-seq re (name (first s))))
                                         name-in (if (nil? name-in) in name-in)
                                         cur-in (if (< (count (re-seq re (name (first s)))) 2)  (name (first s)) cur-in)]
                                     (if (and (.contains
                                               (.toLowerCase cur-in) (.toLowerCase name-in))
                                              (or (nil? curn-in) (nil? n-in)
                                                  (.contains
                                                   (.toLowerCase curn-in) (.toLowerCase n-in)))) (last s))
                                     )
                                   )

                                 sounds))
                get-action (fn [in]
                             (cond (sequential? in) in
                                   (string? in) (find-snd in)
                                   (keyword? in) (let [[_ k n] (re-find #"(?i)([a-z]+)(\d+)*" (name in))]
                                                   (find-snd
                                                    (str
                                                     (if (contains? s-map (keyword k))
                                                       (get s-map (keyword k)) (name k))
                                                         n)))
                                   true in))
                mk-block (fn [action block]
                            (reduce
                             (fn [a c]
                               (if (sequential? c)
                                 (conj (vec (butlast a)) (into (last a) c))
                                 (conj a (get-action c) args)))
                             action
                             (vec block)))
                cur (first phrase)
                is-inst (and (sequential? cur)
                             (or (instance? overtone.studio.inst.Inst (first cur))
                                 (instance? overtone.sc.synth.Synth  (first cur))
                                 (fn? (first cur))))
                is-action (or (and (keyword? cur) (nil? (re-find #"^\d" (name cur))))
                              (string? cur)
                              is-inst
                              (nil? cur))
                is-space? #(and (keyword? %) (re-find #"^\d" (name %)))
                is-arg? #(and (sequential? %) (not (is-space? %)) (keyword? (first %)) (number? (second %)))
                is-arg (is-arg? cur)
                is-space (is-space? cur)
                is-block (and (not is-action) (not is-space) (not is-arg))
                action (get pattern beat [])
                action (cond
                         is-action (if is-inst
                                     (concat action (get-action cur))
                                     (conj action (get-action cur) args))
                         is-arg (conj (vec (butlast action)) (into (last action) cur))
                         is-block (mk-block action cur)
                         true nil)
                pattern (if (and (not (nil? action)) (> (count action) 0))
                          (assoc pattern beat action) pattern)
                space  (cond is-space
                             (-> cur name Integer/parseInt)
                                        ;(not (nil? space)) space
                             true 0)
                pattern (if (and (= (count (rest phrase)) 0) (> space 0))
                          (assoc pattern (+ beat (* space step)) [])
                          pattern)
                beat (if ;(or (is-arg? (second phrase)) (and (not is-space) (is-space? (second phrase))))
                       (or (is-arg? (second phrase)) (is-space? (second phrase)))
                       beat
                       (+ beat (* (if (and is-space (nil? prev)) space (inc space)) step))
                       ;(+ beat (* (if is-space space (inc space)) step))
                       )
                beat (if (= (double (mod beat (int beat))) 0.0) (int beat) beat)]
            (if (> (count (rest phrase)) 0)
              (recur (rest phrase) beat pattern cur)
              pattern
              )
            )))
       )
     {}
     patterns
     )
    ))


(defonce there-there (atom nil))
(swap! there-there
       (fn [_]
         (build-from-kits
          [:Kit3-Acoustic]
          [["Kick-02"] :3
           ["Tom-01" "SdSt-07"] :1
           ["Tom-04"] :2
           ["SdSt-04" "SdSt-03"]
           ["SdSt-07" "SdSt-05"] :3
           ]
          )
         ))




(def t (atom nil))
(swap! t
       (fn [_]
         (let [a [:amp 0.4]
               base (build-from-kits
                     [:Kit3-Acoustic]
                     [["SdSt-03"] :2
                      ["SdSt-06"] ["SdSt-07"] :1 ["SdSt-03" "Snr-04"] :3 ;:1
                      ["SdSt-05" "Snr-09"] :1
                      ["Snr-04" "SdSt-07"] :3]
                     0.25 [:amp 0.4]
                     )]
           (s/m-phrase
            {:refresh 0 :sputter 0.5 :sputter-amt 0.3 :reverse 0.5}
             base 0.25))
         ))



(defn euclid-p [m n action & [rotate-at]]
  (let [init (vec (concat (repeat m [action]) (repeat (- n m) [nil])))
        rotate (fn [scale-sequence offset]
                 (take (count scale-sequence)
                       (drop offset (cycle scale-sequence))))]
    (loop [cur init]
      (let [big (count (first cur))
            small (count (last cur))
            small-cnt (count (filter #(or (< (count %) big) (= % [nil])) cur))
            can-dist (> small-cnt 1)
            to-take (min small-cnt (- (count cur) small-cnt))]
        (if can-dist
          (recur (vec (concat (map #(vec (concat %1 %2))
                                   (take to-take cur)
                                   (take-last to-take cur))
                              (subvec cur to-take (- (count cur) to-take))
                              )))
          (map
           #(if (not (nil? %)) action)
           (if rotate-at
             (rotate (flatten cur) rotate-at)
             (flatten cur))))
        )
      )
    )
  )



(defn gen-beat [base actions
                & [size start-on end-on on-prob off-prob pad step]]
  (let [step (if step step (s/get-step base))
        base-size (inc (/ (dec (s/p-size base step)) step))
        size (if size size base-size)
        pad (if pad pad 1)
        size (loop [size size] (if (= (double (mod size base-size)) 0.0) size (recur (inc size))))
        offsets (range 1 (inc (* (dec (inc size)) step)) step)
        size (+ size (* base-size pad))
        on-prob (if on-prob on-prob 0.1)
        off-prob (if off-prob off-prob 0.1)
        pattern {(inc (* (dec size) step)) []}
        pattern (if start-on (assoc pattern 1 (nth actions (if (number? start-on) start-on 0))) pattern)
        pattern (if end-on (assoc pattern (last offsets) (nth actions (if (number? end-on) end-on 0))) pattern)
        pattern (loop [beats (butlast (rest offsets)) pattern pattern]
                  (let [cur (first beats)
                        cur (if (= (double (mod cur (int cur))) 0.0) (int cur) cur)
                        on-beat (> (count (get base (inc (mod cur (int cur))))) 0)
                        pattern (cond (and on-beat (weighted-coin on-prob))
                                      (assoc pattern cur (choose actions))
                                      (and (not on-beat) (weighted-coin off-prob))
                                      (assoc pattern cur (choose actions))
                                      true pattern)]
                    (if (= (count (rest beats)) 0)
                      pattern
                      (recur (rest beats) pattern))
                    )
                  )]
    pattern
    )
  )



(comment
  (s/add-p core/player there-there :main3)
  (s/set-st core/player (double (/ 1 8)))
  (s/add-p core/player boc-beat :main)
  (s/add-p core/player lazer :pulse)
  (s/add-p core/player untitled-b :switch)
  (s/add-p core/player t :t)
  (start-recorder (mapcat vals
                          (vals (group-samples (drum-kits :KurzweilKit01)))))


  (let [base {2.25 []}
        z [zap [(midi->hz (note :C4))]]
        t [bing [(note :C5) 0.001 0.1 1.5]]
        k [kick []]
        snr [snare [:freq 100 :sustain 0.5 :amp 1]]
        parts {
               :kick [(drum-p [:KurzweilKit04] [:k2 :3])]
               :hat [(drum-p [:KurzweilKit04] [:c :h :o :1 :c :pd :2])]
               }
        rm [:tamb]]
    (doseq [[k [v args]] parts]
      (let [args (if args args [false 0 0.25])]
        (s/add-p core/player
                 v
                   ;; (apply s/fit-p (concat (vector base v)
                   ;;                        args))
                   k))
    )
  ;(s/set-arg @core/s-player :sdst :amp 0.4)
  (doseq [r rm]
    (s/rm-p core/player r)
    )
  )

  (s/add-p
   core/player
   (drum-p
    [:Kit3-Acoustic]
    [:cr3 :cr3 :cr3 :cr3 :cr3 :cr3]
    0.25))


  (doseq [p [:drum1 :drum2 :drum3 :drum4
             :motif1 :motif2 :motif3
             :harmony1 :harmony2 :harmony3]]
    (s/add-p
     core/player
     {2.25 []}
     p))


  (s/get-action-str [sweet])

  (let [patterns [[1 48 :drum1 [:Kit4-Electro]]
                  [1 8 :drum2 [:Kit7-Electro]]
                  [1 8 :drum3 [:Kit15-Electro]]
                  [1 8 :drum4 [:Kit4-Electro]]
                  [1 8 :drum5 [:Kit7-Electro]]
                  [1 8 :drum6 [:Kit15-Electro]]
                  [1 16 :melody1 [:Kit7-Electro]]
                  [1 16 :melody2 [:Kit15-Electro]]
                  [1 48 :harmony1 [:Kit4-Electro]]
                  ;; [1 88 :harmony2 [:Kit4-Electro]]
                  ]]
;    (s/rm-p core/player :all)
    (doseq [[fill slots action kits] patterns]
      (s/add-p
       core/player
       (drum-p
        kits
        (euclid-p fill slots action (rand-int 24))
        0.25 [])
        action)
      )
    )
(s/add-p
   core/player
   (drum-p
    [:KurzweilKit07]
    [:c1 :c1]
    0.25 0 [:amp 0.3])
   :click)

(s/rm-p core/player :click)
  (s/add-p core/player
           {2.75 []} :clap)

  ((drum-s (vector (choose (keys drum-kits))) :k1))

  (s/add-p core/player
           (drum-p [:Kit15-Electro] (euclid-p 5 18 :o) 0.25)
           :hat)

  (s/set-st core/player 0.125)


  (s/set-amp core/player :2 0.6)
  (s/add-p
   core/player
   (fn ([] [7 0.125])
     ([b]
      (let [beat (int (/ (mod b (int b)) 0.125))
            bar (int b)
            action (cond (= beat 6) [(drum-s [:Kit17-Electro] :k2) []]
                         (and (odd? bar) (= beat 4)) [(drum-s [:Kit17-Electro] :s2) []]
                         (and (< bar 5) (odd? bar)) [(drum-s [:Kit6-Electro] :cl2) [] (drum-s [:Kit6-Electro] :cl1) []]
                         )
            action (if (even? beat) (concat action [(drum-s [:Kit4-Electro] :op1) []]) action)
            action (if (and (odd? bar) (even? beat)) (concat action [(drum-s [:Kit5-Electro] :fx2) [] (drum-s [:Kit5-Electro] :fx1) []]) action)
            ]
        action
        )
      ))
   :stream-test)


  (s/add-p
   core/player
   (gen-beat (:four-beat @beats)
             (map #(vector % [:amp 1.5]) (concat (vals (drum-kits :Kit8-Vinyl))
                                                 ;; (vals (:Tom
                                                 ;;        (group-samples
                                                 ;;         (drum-kits :KurzweilKit08))))
                                          ))
             12
             true true 0.5 0.5 1)
   :main)

  (s/rm-p core/player :cr)



  (s/rm-p player :kick)


  (kill trigger-synth)

  (let [base {1 [:k []] 1.75 []}
        sounds (group-samples (drum-kits :Kit3-Acoustic))
        actions [[bing []]]
        samples (map #(vector % [])
                     (vals (merge ;(get sounds :SdSt)
                                  ;(get sounds :ClHat)
                                  (get sounds :Snr)))
                     )
        beat (gen-beat base
                samples
                8 true true 0.3 0.4 0)
        beat (drum-p [:Kit6-Electro]
                     )
        ]
    (s/add-p core/player beat :snr)
    )

  (let [patterns [[:k :k nil nil :k2 nil nil nil :k nil nil [:k :o] nil nil nil :k nil nil]
                  [nil :c nil :c nil :c nil nil :c nil :c nil :c nil :c nil [:c :t2] nil :c nil nil :c nil :c]
                  [:s2 :s nil :s2 nil nil :s nil nil :t :s nil nil nil :s nil nil :s]
                  ]]
    (doseq [p patterns]
        (s/add-p core/player
         (drum-p [:Kit12-Vinyl]
                 p)
         (some #(if (not (nil? %)) % false) p)
         )
        )
    )
  (s/rm-p core/player :o)

  (s/add-p
   core/player
   (drum-p [:Kit16-Electro] [:k :2])
   :g
   )



  (s/mod-p core/player  :g :use-counter true)

  (s/add-p
   core/player
   (let [b [bing [(note :F#4) :amp 1]]]
     (drum-p [:Kit3-Acoustic]
             [:1 b :2 b :1]))
   :b)

  (s/rm-p core/player :test)
  (s/play-p techno1 son-clave 2 3)
  (s/add-p core/player techno1 :main3)
  (s/add-p core/player
           funky-drummer :main2)
  (s/add-p core/player (s/m-phrase {:refresh 0.8 :sputter 0.7 :sputter-amt 0.3}
                                   funky-drummer 0.25) :main2)
  (s/add-p core/player impeach-the-president :main1)
  (s/add-p
   core/player
   (drum-pattern
    [:Kit16-Electro :Kit5-Electro]
    [[k1] :2 [cl1] [s3] :1 [f1] [t1]]
    )
   :main)
  (s/add-p
   core/player
   (drum-p [:Kit3-Acoustic] [:cr3 :cr3 :cr3 :cr3])
   :cr)

  (s/rm-p core/player :sd)
  (s/wrap-p core/player :pulse false)
  )

  (def funky-drummer
    (let [k "Kick-03" s "Snr-04" c "ClHat-04" o "OpHat-01"]
      (build-from-kits
       [:Kit3-Acoustic]
       [[k c] [c] [k c] [c] [s c] [c]
        [k c] [s o] [c] [s c] [k c] [s c] [s c]
        [k o] [c] [s c]]
       0.25 [:amp 0.5])))

  (def techno1
    (let [k "Kick02"
          s "Snr03" c "ClHat02" o "OpHat" cl "Clap02"]
      (build-from-kits
       [:Kit16-Electro]
       [[k] :1 [o] :1 [k s] :1 [o] :1
        [k] [c] [o] :1 [k s] :1 [k o] :1]
       0.25 [:amp 0.5])))

(def son-clave
  (let [k "Kick-03" r "Rim" c "ClHat"]
    (build-from-kits
     [:Kit3-Acoustic :Kit11-Vinyl]
     [[k r c] [c] [c] [k r c]
      [k c] [c] [r c] [k c] [k c]
      [c] [r c] [k c] [k r c] [c]
      [c] [k c]
      ])))

(def bossa-nova
  (let [k "Kick-03" r "Rim" c "Ride02"]
    (build-from-kits
     [:Kit3-Acoustic :Kit11-Vinyl]
     [[k r c] [c] [c] [k r c]
      [k c] [c] [r c] [k c] [k c]
      [c] [r c] [k c] [k c] [r c]
      [c] [k c]
      ])))

(def impeach-the-president
  (let [k "Kick-03" s "Snr-04" c "ClHat-04" o "OpHat-01"]
    (build-from-kits
     [:Kit3-Acoustic]
     [[k c] :1 [c] :1 [s c] :1 [c]
      [k c] [k c] :1 [o] :1 [s c] :1 [k c] :1]
     0.125 [:amp 0.5])))
