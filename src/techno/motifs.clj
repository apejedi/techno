(ns techno.motifs
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.synths]
        [techno.melody]
        [techno.samples]
        [techno.drum-patterns])
  (:require
   [techno.recorder :as rec])
  )


(defonce motif (atom (fn [_])))
(swap! motif (fn [_]
               (s/phrase-p
                ks1
                [:Bb5 :Ab5 :G5 :Ab5 :G5 [:dur 5] :3 :D4 :Eb4 :D4 :1 :C4 [:dur 8] :3]
                0.25 0 [:coef 0.01 :dur 2]
                {:refresh 0.7 :sputter 0.5 :sputter-amt 0.2}
                )
               ))

(def ambient (fn [b]
               (let [n (choose (scale :C5 :minor))
                     dur (s/get-rand-int 1 3)]
                   (if  (= (- b (int b)) 0.5)
                     [bpfsaw [n :dur dur]
                      ])
                   )
               ))
(defonce chords (atom []))

(swap! chords
       (fn [_]
         (let [c #(chord-degree % :C4 :minor 4)]
        (s/phrase-p
         sweet
         [(c :v) (c :ii) (c :vi) (c :i) :3]
         ;[(c :ii) :1 (c :iii) :1 (c :iv) :1 (conj (c :v) (note :C5)) :3]
         0.25 3 [:amp 0 :dur 1 :coef 0.01 :attack 1 :release 2 :vib 0]))
         ))

(defonce arpeggio (atom nil))
(swap! arpeggio (fn [_]
                  (let [root :C4
                        type :minor
                        args [:coef 0.001 :amp 0.4 :atk 0.01 :dur 1]
                        inst ks1
                        v (flatten (repeat 8 (chord-degree :v root type 4)))
                        i (flatten (repeat 8 (chord-degree :iv root type 4)))]
                    (s/arp-p inst (concat v i) args 0)
                    )
                  ))

(def scatterbrain
  (let [inst ks1
        args [:coef 0.001 :dur 2 :atk 0.001 :amp 0.7]
        t [:dur 2]
        s [:space 1]
        main [:C3 :A4 :E5 t s
              :B2 :D4 :E5 t s
              :A3 :F4 :C5 t s
              :D5 :E5 :B4 t s
              ;:G3 :E4 :B4 t s
              ]
        switch [:F#3 :Eb4 :A4 t s
                :E3 :E4 :G4 t s
                :E3 :Eb4 :A4 s
                :B4 s]
        ;; main [:G4 :A4 :E5 t s
        ;;       :F#4 :A4 :D5 t s
        ;;       :E4 :G4 :C5 t s
        ;;       :D4 :F#4 :B4 t s]
        ;; switch [:E4 :F#4 :A4 t s
        ;;         :E4 :F#4 :G4 t s
        ;;         :E4 :F#4 :A4 t s
        ;;         :B4 s]
        switch2 [:G4 :B4 :Eb5 t s
                 :D5 t s
                 :C5 t s
                 :F5 t s]]

    (s/phrase-p
     inst
     (concat
      main
      main
      switch
      ;switch2
      )
     (double (/ 1 4))
     0
     args
     )
    ))


(def coffee (atom nil))
(swap! coffee
       (fn [_]
         (let []
             (s/phrase-p
              piano
              [
               [:F#5 :A5 :F#6]
               [:F#5 :B6]
               [:F#5 :C#6]
               ;; [:F#4 :A4 :F#5] :F4
               ;; [:Ab4 :B4 :Ab5] :F4
               ;; [:A4 :C#4 :A5] :F4
               ;; [:F4 :A4 :F5] :F4
               ]
              (double (/ 1 4))
              0
              [:dur 2 :amp 0.7 :coef 0.01]
              ))
         ))

(defn rnd-chord
  ([] [7.75 0.25])
  ([b]
   (if (= b 1)
     (s/chord-p
      rise-pad
      (map midi->hz
           (chord-degree
            (choose [:i :v :vi :iii])
            :B3
            :major 4))
      [:coef 0.01 :amp 0.2 :dur 2 :attack 0.1 :release 4 :t 7]
      ))
   )
  )

(comment
  (s/play-p
   (s/phrase-p
    bowed
    [:C4 [:dur 0.7 :start 0 :end 0.5] :3 :G4 [:dur 0.7 :start 0.7 :end 0] :3]
    0.25 0))
  (mapcat (fn [k] [(drum-s [k] :c1) (drum-s [k] :c2)])
                                          [:Kit4-Electro
                                           :Kit5-Electro
                                           :Kit15-Electro
                                           :Kit16-Electro
                                           :KurzweilKit01
                                           :KurzweilKit02
                                           :KurzweilKit05])
  (s/add-p core/player scatterbrain :sc)
  (s/add-p core/player ambient :harmony)
  (s/add-p core/player melissa :harmony)
  (do
    (zap (midi->hz (note :C4)) (midi->hz (note :Bb3)) :dur 1)
    (zap (midi->hz (note :F3)) (midi->hz (note :G4)) :dur 1)
    )

  (let [[i iv v vi] (map #(map midi->hz
                               (chord-degree
                                %
                                :C4
                                (choose [:minor]) 4)) [:i :iv :v :vi])]
    (s/add-p core/player
             (s/fit-p {1.75 []}
              (s/phrase-p
               rise-pad
               [iv v vi i :15]
               0.25 15)) :motif)
    )

  (s/add-p
   core/player
   {1 [(fn [& args]
           (doseq [n (chord-degree (choose [:ii :iv :v]) :C4 :minor)] (overpad n) )) []]}
   :test)


  (s/add-p
   core/player
   (s/phrase-p
    bing
    (vec (map #(nth (scale :C4 :minor) (mod % 7))
              [1 5 3 7]))
    0.125 1 [:decay 1])
   :harmony2)

  (apply (fn [& args]
           (doseq [n (chord-degree (choose [:ii :iv :v]) :C4 :minor)] (overpad n) )) [])
  (s/add-p
   core/player
   (s/phrase-p
    rise-fall-pad
    [(map midi->hz (chord :C3 :M7)) (map midi->hz (chord :G3 :M7)) :34]
    0.25 32 [:t 5])
   :harmony)

  (s/add-p
   core/player
   (fn [b]
     (if (or (integer? b) (weighted-coin 0.3))
       [piano [(choose (scale :C5 :major)) :dur 3]])
    )
   :motif)

  (let [b [bass [:amp 1.3 :cutoff 4000 :t 0.3]]
        c [bass2 [:freq 150 :amp 1.3]]]
    (s/play-p
     (s/build-map-p
      [b b b b :1 b :1 c c :3]
      0.25)
     2 3)
    )

  (def chi (chicago-pad :freq (midi->hz (note :C3)) :amp 0.1))
  (kill chi)
  (rec/start-record-pattern)
  (ctl 1003 :amp 0.4)
  (let []
    (on-event [:midi :control-change]
              (fn [m]
                (let [types {1 :modulation 2 :breath}
                      type (:data1 m)
                      value (:data2 m)]
                  (println type " " value)
                  )
                )
              ::control-change)
      (on-event [:midi :note-on]
                (fn [m]
                  (let [root "C"
                        cur-scale (map find-pitch-class-name
                                       (scale (keyword (str root "4")) :major))
                        info (note-info (find-note-name (:data1 m)))
                        degree (inc (.indexOf cur-scale (:pitch-class info)))
                        num 4
                        degree-map (zipmap (vals DEGREE) (keys DEGREE))
                        notes (if (contains? degree-map degree)
                                (chord-degree (get degree-map degree)
                                              (keyword (str root (:octave info))) :major num)
                                [])
                        pat (if (> (count notes) 0)
                              (s/build-map-p
                               (map #(let [f (midi->hz %)]
                                       (vector
                                        reverb-test [:freq f ]
                                        )
                                       ) notes)
                               0.25))
                        chan (inc (:channel m))
                        ctr-map {
                                 3 [piano [:note (:midi-note info)   :dur 2 :amp 0.3]
                                    ]
                                 2 [prophet [:freq (midi->hz (:midi-note info)) :attack 1 :decay 2]]
                                 4 [bpfsaw2 [:freq (midi->hz (:midi-note info)) :lsf 1000]
                                    ]
                                 1 [ks1 [:note (:midi-note info) :coef 0.001 :dur 2]]
                                 5 [bpfsaw [:note (:midi-note info) :dur 2.3 :amp 1 :atk 0.3 :rq 0.5]]
                                 6 [bass2 [:freq (midi->hz (:midi-note info)) :decay 4]]
                                 7 [rise-fall-pad2 [:freq (midi->hz (:midi-note info))]]
                                 }]
                    (doseq [[inst args] (partition 2 (get ctr-map chan))]
                      (rec/record-action [inst args])
                      (apply inst args)
                      )
                    ;; (if (= (rand-int 4) 0)
                    ;;     (osc-send @techno.controller/client
                    ;;               (str "/fundamental")
                    ;;               (str (midi->hz (note (keyword (str (name (:pitch-class info)) 4)))))))

                    ;; (when (= 2 (:channel m))
                    ;;   (ctl 1673 :freq (midi->hz (:7midi-note info))))
                    ;; (when (= 3 (:channel m))
                    ;;   (ctl 1672 :freq (midi->hz (:midi-note info))))
                    ;; (plk-bass :note (:midi-note info) :dur 0.8)
                    ;; (s/mod-actions core/player :bass
                    ;;                (fn [[action args]]
                    ;;                  (if (= (:name action) "plk-bass")
                    ;;                    (vector action [:note (:midi-note info)])
                    ;;                    (vector action args)
                    ;;                    )))
                    ;; (when (not (nil? pat))
;; ;                      (s/play-p pat)
;;                       (s/add-p core/player pat (keyword (:match info)))
;;                       )
                    ))
                ::prophet-midi)

                                        ;      (remove-event-handler ::prophet-midi)
    (on-event [:midi :note-off]
              (fn [m]
                ;; (kill bass-synth)
                (let [n (find-note-name (:data1 m))]
                  (when (and (sequential? (core/get-patterns))
                             (>= (.indexOf (core/get-patterns) n) 0))
                    (s/rm-p core/player n)))
                )
              ::prophet-midi-off))
  (s/add-p core/player
           (s/phrase-p
 klang-test
[:G6 :2 :D6 :11 :C6 :5 :Eb6 :5 :D6 :4]
0.25 0 [:atk 0.01 :amp 7.3]) :motif1)
  (s/play-p
   (s/phrase-p
    bass-synth
    [(chord :C4 :M7) (chord :E4 :m7) (chord :A3 :m9)]
    0.25 6 [:attack 2 :release 2]))
  (s/play-p
   (s/build-map-p
    [[o-kick []] :3]
    0.25 0)
   (s/build-map-p
    [:4 [o-clap []
         (drum-s [:Kit15-Electro] :cl1) []] :3]
    0.25 0)
   ;; (s/build-map-p
   ;;  [:8 [o-snr []] [o-snr []] [o-snr []] [o-snr []] :2]
   ;;  0.25 0)
   (drum-p
    [:Kit4-Electro :Kit2-Acousticroom]
    [:c1 :c2]
    0.25 0 [])
   (s/phrase-p
    acid-bass
    [:C2 :Eb1 :D2 :C2 :2]
    0.25 0 [:amp 0.3 :dur 0.3])
   (s/phrase-p
    bass-synth
    [[:C4 :F3] :Eb3]
    0.25 1 [:amp 0.6 :decay 0.9 :attack 0.01])
   2 6)

(remove-event-handler :test-midi)
  (s/add-p
   core/player
   (s/phrase-p
    bass2
    [:E5 :D5 :C5 :B4 :6]
    0.25 2 [:decay 2 :cutoff2 4000])
   :bass3)
  (rec/start-record-pattern)
  (s/pp-pattern (rec/get-time-pattern))
  (kill 1006)

  (let [b [;bass2 [:decay 1 :cutoff2 3000]
           plk-bass [:amp 0.4]
           wire-bass [:amp 0.4]]
        ]
      (s/add-p core/player
               (s/build-map-p
                [b b]
                0.25)
               :bass))

  (s/add-p
   core/player
   (s/phrase-p
    acid-bass
    [:C4 :D4 :E4 :C4 :D4 :3 :E4 :F4 :E4 :D4 :6]
    0.25 2 [:dur 0.6 :amp 0.3])
   :bass2)
  (s/rm-p core/player :bass2)
(remove-event-handler ::prophet-midi)

(s/play-p
 (s/phrase-p
  overpad
  [(chord :B3 :m9) :6 (chord :A3 :M7)]
  0.25 0 [:attack 1 :release 3]))


(s/play-p
 (s/phrase-p
  b-kick
  [:D4 :3 :D4 :2 :F4 :C4 :3 :B4 :3]
  0.25 0)
 (s/build-map-p
  [:3 [o-hat []] :1 [o-hat []] :2])
 (s/build-map-p
  [:4 [(drum-s [:Kit15-Electro] :cl1) []] :3])
 (s/build-map-p
  [:2 [b-snr [:note (note :D4) :dur 0.5] o-snr []] :1])
 (let [w [acid-bass [(note :D1) :dur 0.5 :amp 0.5]]
       b [bass2 [:amp 2]]]
     (s/build-map-p
      [:7 w :1 w w :1 b :1 w b]))
 2 4)

  (let [mk-prog (fn [p]
                (conj
                 (vec
                  (mapcat #(vector (chord-degree % :C4 :major)) p))))
      a (mk-prog [:ii :iii :i])
      b (mk-prog [:iv :iii :i])]
    (s/play-p
     (s/phrase-p
      piano
      (concat a b)
      0.25 1 [:hard 0.3 :dur 2])))

  (s/play-p
   (let [a (map #(vector :freq1 (midi->hz %1)
                         :freq2 (midi->hz %2) :amp 0.8) (chord :C4 :m7) (chord :G4 :m7))]
     [[whistle (nth a 0) whistle (nth a 1) whistle (nth a 2)]]
     ))

  (demo
   )






(s/add-p core/player {1 [tick []] 6.25 []} :total)
  (s/add-p
   core/player
   (s/phrase-p
    prophet
    [:Ab4 :C#5 [:Bb4 :B3]]
    0.25 0 [:sustain 0.5 :decay 0.1 :attack 0.1 :amp 0.5 :cutoff-freq 2000]
    )
   :phrase
   )

  (s/add-p
   core/player
   (s/phrase-p
    bass-synth
    [:C#2 :F#2 [:Bb2 :B3] :A3]
    0.25 3 [:sustain 2 :decay 0.1 :attack 1 :amp 0.3 :cutoff-freq 4000]
    )
   :phrase2
   )
  (s/add-p
   core/player
   (s/phrase-p
    piano
    [:D3 :3 :A3 :3 :G4 :3] 0.25 0 [:amp 0.5]) :piano)
  (sweet [:head 5] :dur 2)
  (ctl 802 :volume 1)
(s/rm-p core/player :phrase2 true)


(conj [3 4] [:rv 2])


  (s/add-p
   core/player
   (s/phrase-p
    bass-synth
    [:F3 :C#3 :Bb2 :B3]
    0.25 0 [:amp 1.5 :attack 0.01 :release 0.5])
   :bass)

  (s/rm-p core/player :phrase2)

  (s/mod-p core/player :clap :use-counter true)

  (let [a (vec (map note [:G2 :D3 :F#3]))
        r (range 0 3)
        b (shift a r -2)
        c (shift a r 3)
        d (shift a r 1)
        ]
    (s/play-p
     (s/phrase-p
      piano
      [a a a a :1 (shift a [2] 1) :1 a b b b b
       c c c c d d d d]
      0.25 3 [:attack 0.1 :release 0.3 :dur 2 :hard 0.01 :velhard 0.1])
     1.6
     )
    )


  (s/set-arg core/player :click :amp 0.2)

  (s/add-p core/player coffee :harmony)
  (s/add-p core/player untitled :harmony)
  (s/add-p core/player untitled-f :motif)
  (s/add-p core/player rnd-chord :motif)

  (let []
    (s/add-p
     core/player
     (s/phrase-p
      plk-bass
      [:Eb3]
      0.25 3)
     :bass)
   )
  (s/play-p
   (s/phrase-p
    bpfsaw
    [:Bb4 :G4 :F4]
    0.25 1 [:dur 1]))


  (s/add-p
   core/player
   (fn [b]
     (if (or (= (rand-int 4) 0)
          (integer? b) (= (mod b (int b)) 0.5) (= (mod b (int b)) 0.75)
          )
       (let [notes (concat (chord-degree :ii :C4 :minor 4) (chord-degree :iii :C4 :minor 4)
                           (chord-degree :v :C4 :minor 4))]
         [(choose [piano]) [(choose notes) :dur 1 :amp 0.2 :coef 0.05]
          (choose [piano]) [(choose notes) :dur 1 :amp 0.15 :coef 0.05]]
         )))
   :motif2)

  (s/set-amp core/player :a 0.2)
  (s/set-arg core/player :main3 :amp 0.5)
  (s/play-p acid ;core/player acid :acid ;{:sc303 1001}
            5)
  (ctl t :gate 0)
  (ctl t :gate 1 :freq 100 :dec 4 :sus 1 :wave 0)

  (s/rm-p core/player :harmony)
  (s/add-p core/player chords :harmony)
  (s/add-p core/player arpeggio :arp)
  (s/rm-p core/player :harmony)
  (s/wrap-p core/player :harmony true)
  (s/add-p core/player ted-guitar :guitar)
  (s/add-p core/player x-naut :x-naut)

  (s/add-p
   core/player
   (let [[a b] [[:G3 :D4] [:G3 :Eb4]]
         [c d] [[:A3 :E4] [:A3 :F4]]]
       (s/phrase-p
        ks1
        [a b a b :2 a b :2 a b a b a b :2
         c d c d :2 c d :2 c d c d c d :2]
        0.25 0 [:coef 0.01 :dur 2]))
   :motif2)
  (s/rm-p core/player :motif2)

  (let [notes (atom (s/x-seq (scale :B4 :minor)))]
    (s/add-p
     core/player
     (fn [b]
       (reset! notes (rest @notes))
       [(choose [bpfsaw piano]) [(first @notes) :dur 0.7 :atk 0.1]])
     :motif))

  (let [notes [:C4 :D4 :A4 :Bb4]]
    (s/phrase-p
     bing
     notes
     [:decay 2]))

(s/set-st core/player 0.25)

  (s/play-p
   ;core/player
   (let [root :C4
         n 4
         [a b c d e f g]
         (scale root :minor)
         ;(map #(chord-degree % root :minor n) [:i :ii :iii :iv :v :vi :vii])
         ]
     (s/phrase-p
      bpfsaw
      [b d b b f a c a g]
      0.25 0
      [:dur 0.7 :atk 0.01]
      ))
   (let [root :C3
         n 4
         [a b c d e f g]
         (map #(chord-degree % root :minor n) [:i :ii :iii :iv :v :vi :vii])
         ]
     (s/phrase-p
      bass-synth
      [g a b a f]
      0.25 6
      [:attack 0.02 :release 1.7 :cutoff-freq 1000 :amp 0.3]
      ))
   1.3
   2
   ;:motif
   )

  (s/add-p
   core/player
   (let [a (chord :C4 :M7)
         b (chord :B3 :M7)
         ]
     (s/phrase-p
      klang-test
                                        ;[a a a a a :0 b b b b b :1]
      [a a a a a
       :0 b b b b b :2
       ]
      0.25 2 [:dur 0.8 :amp 0.5 :vib 0]))
   :harmony1
   ;1.6
   )
  (daf-bass)
  (kill daf-bass)
  (s/set-action core/player :harmony1 7.875 [])

  )




(def untitled
  (let [[c b d a] (map #(vector (note %) (- (note %) 4)) [:C4 :Bb3 :D4 :A3])
        -- [:space 2]
        | [:space 0]
        phrase [c -- c -- c c
                b -- b | b -- b  b  b
                d -- d -- d  d
                a -- a | a -- a  a  a]]
    (s/phrase-p
     piano
     phrase
     (double (/ 1 4))
     1
     [:dur 2 :amp 0.3]))
  )
(def untitled-f
  (let []
    (s/phrase-p
     flute
     [:D4 :Eb4 :F4 :G4 :F#4 :Ab4 :Bb4 [:space 3]]
     (double (/ 1 4))
     6
     [:dur 1.5]
     )
    ))

(def ted-guitar
  (let [inst piano
        args [:coef 0.01 :dur 2]]
    (s/phrase-p
     inst
     [(chord :G4 :m7)
      (chord :F#3 :M7)]
     0.25
     2
     args
     )
    )
  )



(def acid
  (let [inst sc303
        f #(vector [inst [:freq % :gate 1]]
                   [inst [:gate 0]])
        freqs [65.40639132515
               77.78174593052
               48.999429497719
               73.416191979352
               65.40639132515
               65.40639132515
               130.8127826503
               65.40639132515
               65.40639132515
               32.703195662575
               65.40639132515
               65.40639132515
               65.40639132515
               32.703195662575
               65.40639132515]]
    (into [[sc303 [:freq 65.406395 :env 5000.0 :sus 2 :ctf 1294.8276 :res 1.0 :dec 1.4827586 :wave 1.0 :vol 0.2]]]
          (mapcat f freqs))
    )
  )

;; (fn
;;   ([] [12.75 0.25])
;;   ([b]
;;    (if (= b 1)
;;      (let [notes (chord-degree (choose [:i :ii :iii :vi]) :C3 :major (choose [3 4]))]
;;        (doall
;;         (map (fn [c n] (let [resp (osc-send
;;                                   @techno.controller/client
;;                                   (str "/fundamental/" c)
;;                                   (int n))])
;;                )
;;              (range 0 (count notes))
;;              notes


;;              )
;;         )
;;        (s/chord-p
;;         bass-synth
;;                                         ;(chord-degree (choose [:i :vi :iii :iv]) :C4 :major 4)
;;         (map midi->hz notes)
;;         [:coef 0.01 :t 10 :attack 8 :release 5 :amp 0.1]))
;;      )))

(let [state (atom {})
      bpm 120
      step 0.25
      fps 24
      n-len (int (/ (* fps 60 step) bpm))
      fired (atom false)
      ;; f-map (loop [m {} offsets (range 0 24)]
      ;;         (if (> (count offsets) 0)
      ;;           (recur (assoc m offset)
      ;;                  (rest offsets)
      ;;                  )))
      ]
    (on-event
     [:midi nil]
     (fn [m]
       (when (contains? m :msg)
         (let [type (bit-shift-right (bit-and (second (.getMessage (:msg m))) 2r11110000) 4)
               val (bit-and (second (.getMessage (:msg m))) 2r00001111)]
           (swap! state assoc type val)
           (when (= type 3)
             (let [frame (bit-or (bit-shift-left (get @state 1) 4) (get @state 0))
                   second (bit-or (bit-shift-left (get @state 3) 4) (get @state 2))]
               ;; (when (and (>= frame 0) (< frame 4) (not @fired))
               ;;   (o-kick)
               ;;   (reset! fired true)
               ;;   )
               ;; (when (and (>= frame 4) (< frame 10) @fired)
               ;;   (reset! fired false)
               ;;   )
               ;; (when (and (>= frame 12) (< frame 15) (not @fired))
               ;;   (o-kick)
               ;;   (reset! fired true)
               ;;   )
               ;; (when (and (> frame 15) (< frame 24) @fired)
               ;;   (reset! fired false)
               ;;   )
                                        ;(println second frame)
               (println "frame " frame ;(bit-or (bit-shift-left (get @state 1) 4) (get @state 0))
                        "second " second ;(bit-or (bit-shift-left (get @state 3) 4) (get @state 2))
                        )
               )
             )
           )
         )
       )
     :midi-clock))

                                        ;(remove-event-handler :midi-clock)

(let [started (atom false)
      counter (atom 0)
      beat (atom 1)]
    (on-event
     [:midi nil]
     (fn [m]
       ;(println (:status m) " " (first (.getMessage (:msg m))) " " (second (.getMessage (:msg m))))
       (when (and (not @started) (= (:status m) :start))
         (reset-s core/player)
         (reset! started true)
         (println "syncing"))
       (when (= (:status m) :stop)
         (println "stopping")
         (reset! started false))
       )
     :midi-clock))
