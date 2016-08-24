(ns techno.drums
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.samples]
        [techno.recorder]
        [techno.synths]
        )
  (:require [techno.sequencer :as s]
            [clojure.string :as string])
  )



(def drum-kits (create-sample-map (str (.getCanonicalPath (clojure.java.io/file ".")) "\\musicradar-drum-samples\\Drum Kits") true))


;;Functions to generate patterns


(defn gen-pattern [size instruments]
  (let [pattern {}]
    (reduce
     (fn [pattern beat]
       (let [instrument (choose instruments)]
         (assoc pattern
                beat
                (vector instrument [])
                )))
     {}
     (range 1 (inc size))
     )
    )
  )

(defn categorize-kit [kit]
  (let [sounds (drum-kits kit)
        types [:kicks  :snrs :cymbals :toms  :ophats :clhats :claps :fxs :hfhats]]
    (reduce (fn [s c]
              (assoc s c (into {}
                               (map
                                #(if (.contains (string/lower-case (name (first %)))
                                                (apply str (butlast (name c))))
                                   [(-> (re-seq
                                         #"-([A-Za-z-\d]+)[^\dA-Za-z]?\..*$" (name (first %)))
                                        first last keyword)
                                    (second %)
                                    ])
                                sounds)))
              )
            {}
            types)
    )
  )

(defn build-from-kits [kits pattern & [step args]]
  (let [sounds (reduce into [] (map #(drum-kits %) kits))
        args (if args args [])
        get-inst (fn [in]
                   (cond (sequential? in) in
                         true (if (string? in)
                                (vector
                                 (some (fn [s]
                                         (let [re #"(?i)([a-z]+)[^0-9a-z]*([0-9]+)"
                                               [res name-in n-in] (last (re-seq re in))
                                               [cur cur-in curn-in] (last (re-seq re (name (first s))))]
                                           (println name-in n-in cur-in curn-in)
                                           (if (and (.contains
                                                        (.toLowerCase cur-in) (.toLowerCase name-in))
                                                    (or (nil? curn-in) (nil? n-in) (.contains
                                                                                       (.toLowerCase curn-in) (.toLowerCase n-in)))) (last s))
                                           )
                                          )
                                       sounds)

                                 args))
                         ))
        s (fn [ins]
            (if (or (and (sequential? ins) (= (first ins) :space)) (keyword? ins))
              ins
              (mapcat get-inst ins)))]
    (cond (map? pattern) (zipmap (keys pattern) (map s (vals pattern)))
          (sequential? pattern) (s/build-rest-p (map s pattern) step)
          )
    )
  )
                                        ;Patterns




(defonce beat (atom {}))
(swap! beat (fn [_]
              (build-from-kits [:Kit10-Vinyl]
                               {
                                1 ["Kick01" "ClHat01"]
                                1.25 ["Kick04"]
                                1.5 ["Perc04"]
                                2 ["Snr02"]
                                2.5 ["Perc03"]
                                ;3 ["OpHat"]
                                4 ["Snr02"]
                                4.75 []
                                }
                               )
              ))


(defonce syncop (atom nil))
(swap! syncop (fn [_]
                (fn
                  ([] [4 0.25])
                  ([b]
                   (let [kit (categorize-kit :Kit15-Electro)]
                     (if (not (integer? b))
                       [(choose (concat
                                        ;(vals (:kicks kit))
                                 (vals (:claps kit))
                                 (vals (:snrs kit))
                                 )
                                ) [:amp 0.4]]
                       )))
                  )
                ))

(defonce electro (atom {}))
(swap! electro
       (fn [_]
         (build-from-kits [:Kit10-Vinyl :Kit15-Electro]
                          {
                           1 ["Perc01"]
                           1.5 ["ClHat01"]
                           ;; 2 ["Perc01"]
                           ;; 2.25 []
                           })
         ))

(defonce pulse-beat (atom []))
(swap! pulse-beat (fn [_]
                    (build-from-kits [:Kit10-Vinyl]
                          {
                           1 ["Perc02"]
                           1.25 ["Perc03"]
                           2 ["Perc04"]
                           2.75 []
                           }
                          )))

(def categorized-kit (categorize-kit :Kit3-Acoustic))
(def random-beat
  (fn [b]
    (let [sounds categorized-kit]
      (cond (integer? b)
            (cond (odd? b) [(-> (:kicks sounds) vals first) []]
                  true [(-> (:kicks sounds) vals second) []])
            ;true [(choose (concat (vals (:cymbals sounds)) (vals (:toms sounds)))) []]
            (= (- b (int b)) 0.5)
            [(choose (concat
                      (vals (:hats sounds))
                      (vals (:toms sounds)))) []]
        )
      )
    )
  )


(comment
  (start-recorder (mapcat vals
                          (vals (group-samples (drum-kits :Kit3-Acoustic)))))
  (s/set-sp core/player (/ 80 60))
  (s/set-size core/player 4.25)
  (s/add-p core/player electro :electro)
  (s/add-p core/player pulse-beat :pulse)
  (do
    (s/rm-p core/player :harmony)
    (s/rm-p core/player :electro)
    (s/rm-p core/player :motif)
    ;(s/rm-p core/player :arp)
    (s/rm-p core/player :bass-line)
    )
  (do
    (s/add-p core/player techno.motifs/arpeggio :arp)
    (s/add-p core/player (:four-beat @beats) :main)
    )
  (s/add-p core/player beat :main)
  (s/add-p core/player (:bomba @beats) :main)
  (s/add-p core/player (:four-beat @beats) :main)
  (s/add-p core/player syncop :syncop)

  (s/rm-p core/player :main)
  (s/rm-p core/player :electro)
  (s/rm-p core/player :syncop)
  (s/add-p core/player scatter-pulse :pulse)
  (s/add-p core/player scatter-main :main)
  (s/add-p core/player untitled :main)
  (s/add-p core/player untitled-b :switch)
  (s/mod-p core/player :switch :min-wrap 2)
  (s/play-p untitled-b untitled 2)
  )





(def untitled (build-from-kits
                [:Kit3-Acoustic]
                {1 ["Kick-02"]
                 1.125 ["Tom-01"]
                 1.25 ["Tom-04"]
                 ;; 1.375 ["Tom-04"]
                 ;; 1.5 ["Tom-01"]
                 ;; 1.625 ["Tom-04"]
                 2 ["Rim-01"]
                 3 ["Rim-01"]
                 3.75 []
                 ;; 2.75 ["Kick-02"]
                 ;; 2.875 ["Tom-04"]
                 ;; 3 ["Kick-02"]
                 ;; 4 []
                 }))

(def untitled-b (atom nil))
(swap! untitled-b
       (fn [_]
         (build-from-kits
          [:Kit3-Acoustic]
          {1 [[dance-kick []]]
           1.5 ["SdSt-03"]
           2 ["Snr-07"]
           2.25 ["SdSt-03" "Snr-03"]
           }
          )
         )
       )
(defonce beats (atom {}))
(swap! beats (fn [_]
               {:one-two
                (build-from-kits [:Kit5-Electro]
                                 {
                                  1 ["Kick02"]
                                  2 ["Snr02"]
                                  2.75 []
                                  }
                                 )
                :three-beat (build-from-kits
                             [:Kit10-Vinyl]
                             {1 ["Kick01"]
                              1.75 ["Snr01"]
                              2.25 ["Kick01"]
                              3 []
                              })
                :four-beat (build-from-kits
                             [:Kit10-Vinyl]
                             {1 ["Kick01"]
                              1.5 ["ClHat01"]
                              2 ["Snr02"]
                              2.5 ["ClHat01"]
                              2.75 []
                              })
                :three-four (build-from-kits
                             [:Kit10-Vinyl]
                             {1 ["Kick04"]
                              1.75 ["Snr02"]
                              2.25 ["Kick04"]
                              2.75 ["Snr02"]
                              })
                :six-eight (build-from-kits
                             [:Kit10-Vinyl]
                             {1 ["Snr02"]
                              1.75 ["Kick04"]
                              2 ["Snr02"]
                              2.25 ["Kick01"]
                              2.75 []
                              })
                :bomba (build-from-kits
                             [:Kit10-Vinyl]
                             {1 ["Kick01" "Kick04"]
                              1.5 ["Perc01"]
                              1.75 ["Kick01"]
                              2 ["Kick02"]
                              2.25 ["Perc01"]
                              2.5 ["Kick01"]
                              2.75 []
                              })
                }
               ))

(def scatter-pulse
  (build-from-kits
   [:Kit3-Acoustic]
   {1 ["Crash-01"]
    }
   ))
(def scatter-main
  (build-from-kits
   [:Kit3-Acoustic]
   {1 ["SdSt-05"]
    1.25 ["SdSt-05"]
    1.5 ["SdSt-05"]
    1.75 ["SdSt-07" "SdSt-06"]
    2 ["SdSt-05"]
    2.25 ["SdSt-05"]
    2.5 ["SdSt-07" "SdSt-06"]
    2.75 ["SdSt-05"]
    3 ["SdSt-05"]
    3.25 ["SdSt-05"]
    }
   ))
