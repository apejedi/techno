(ns techno.sequencer
  (:use [overtone.core]
        [overtone.inst.drum]
        )
  )



(def pattern
  {1 [dance-kick [:decay 2 :freq 100]]
   2 [closed-hat [] haziti-clap []]
   3 [dance-kick [200] dance-kick [300] ]
   4 [dance-kick [250] ]
   }
  )

(defn gen-pattern [size]
  (let [pattern {}
        instruments [dance-kick closed-hat tone-snare bing]]
    (reduce
     (fn [pattern beat]
       (let [instrument (choose instruments)
             scale (scale (choose [:Bb4 :C#5]) (choose [:major :minor]))
             ]
         (assoc pattern
                beat
                (vector
                 instrument
                 (if (= instrument bing)
                   [:decay (- 2 (rand 1.5))
                    :freq (midicps (choose scale))
                    ]
                   [])
                 )
                )))
     {}
     (range 1 size)
     )
    )
  )

(defn player
  [cur-beat]
  (let [beat-actions (pattern (int cur-beat))]
    (dorun
     (for [[instrument args] (partition 2 beat-actions)]
       (do
         (apply instrument args)
         )
       ))
    )
  )

(def pattern (gen-pattern 10))
(def t (start-sequencer 10 30))
(ctl t :clock-speed 10)
(ctl t :beats-per-measure 10)
(kill t)



(defsynth sequencer [clock-speed 128 uid 0 beats-per-measure 4]
  (let [trigger (impulse:kr clock-speed)
        count (stepper:kr trigger :min 1 :max beats-per-measure)]
    (send-trig:kr trigger uid count)
    )
  )


(defn start-sequencer
  "Creates and starts a sequencer
  args: [clock-speed 1(per second) beats-per-measure 4]
  "
  ([] (start-sequencer 1 4))
  ([clock-speed] (start-sequencer clock-speed 4))
  ([clock-speed beats-per-measure]
   (let [
         uid (trig-id)
         s (sequencer clock-speed uid beats-per-measure)
         ]
     (on-trigger uid (fn [beat] (player beat)) ::sequencer)
     s
     )
   )
  )
