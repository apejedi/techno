(ns techno.drum-patterns
  (:use [techno.drums]
        [techno.sequencer :as s :exclude [t]]
        [techno.core :as core]
        [techno.synths]
        [overtone.core]
        [overtone.inst.synth]
        [techno.samples]
        [techno.recorder]
        )
  )

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



(def boc-beat
  (build-from-kits
   [:Kit3-Acoustic :Kit10-Vinyl]
   {1 ["Tom-01" [dub-kick [200]]]
    1.75 ["Tom-04" [dub-kick [150]]]
    2 ["Tom-05" [dub-kick [100]]]
    2.25 ["Tom-04" [dub-kick [200]] "Rim-01"]
    3 ["Tom-01" [dub-kick [200]]]
    3.5 ["Tom-04" [dub-kick [150]]]
    4 ["Tom-05" "Snr-04" [dub-kick []] "SdSt-07"]
    4.75 []
    }
   )
  )

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


(def lazer (let [d [zap [3000 :amp 0.3 :dur 0.1] dub-kick [300 :amp 1.5]]
                 base (s/build-rest-p
               [d :6 d :2 d :5])]
             (s/m-phrase
              {:refresh 0.7 :sputter 0.7 :sputter-amt 0.3}
              base
              0.25)
             ))

(defn test-drums [patterns play & [times kits speed]]
  (let [kits (if kits kits [:Kit3-Acoustic :Kit16-Electro :Kit10-Vinyl])
        a [:amp 0.5]
        speed (if speed speed 1.3)
        times (if times times 1)]
    (if play
      (apply s/play-p
             (concat (map (fn [[k v]] (if (> (count v) 0) (drum-p kits v))) patterns)
                   [speed times]))
      (doseq [[k v] patterns]
                                        ;(s/pp-pattern (drum-p kits v))
        (if (> (count v) 0)
          (s/add-p
           core/player
           (drum-p kits v)
           k)
          (s/rm-p core/player k))
        ))
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
                          (vals (group-samples (drum-kits :Kit5-Electro)))))






  (let [base {2.25 []}
        z [zap [(midi->hz (note :C4))]]
        t [bing [(note :C5) 0.001 0.1 1.5]]
        k [kick []]
        snr [snare [:freq 100 :sustain 0.5 :amp 1]]
        parts {
               :kick [(drum-p [:Kit17-Electro] [:k1 :5])]
               :toms [(drum-p [:Kit5-Electro] [:1 :t2 :t2 :7])]
               :crash [(drum-p [:Kit3-Acoustic] [:cr3 :cr4 :cr4 :1 [:o1 :o2] :5])]
               :snr [(drum-p [:Kit3-Acoustic] [:3 :s2 :1 :s2 :4])]
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





  (let [patterns [[3 8 :k [:Kit4-Electro]]
                  [12 20 :c [:Kit16-Electro]]
                  [5 20 :s [:Kit7-Electro]]
                  ]]
;    (s/rm-p core/player :all)
    (doseq [[fill slots action kits] patterns]
      (s/add-p
       core/player
       (drum-p
        kits
        (euclid-p fill slots action)
        0.25 [])
        action))
    )

  (s/set-st core/player 0.125)


  (s/set-amp core/player :2 0.6)



  (s/add-p
   core/player
   (gen-beat (:four-beat @beats)
             (map #(vector % [:amp 1.5]) (concat (vals (drum-kits :Congas))
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
