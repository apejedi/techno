(ns techno.core
  (:use [overtone.core]
        [techno.sequencer]
        [techno.pattern]
        [techno.synths]
        )
  )

(defonce sequencer (atom nil))


(defn connect-sc
  ([] (connect-sc 57110))
  ([port] (connect-external-server port))
  )

(defn init []
  (connect-sc)
  )


(defn disconnect-sc []
  (overtone.sc.machinery.server.connection/shutdown-server)
  )

(defn pause-seq []
  (stop-seq @sequencer)
  )

(defn play-seq [bpm]
  (if (and (not (nil? @sequencer)) (is-running? @sequencer))
    (pause-seq))
  (swap! sequencer (fn [_] (get-seq bpm)))
  )


(defn add-pattern [p key]
  (add-p @sequencer p key)
  )

(defn remove-pattern [key]
  (rm-p @sequencer key)
  )

(comment
  (init)
  (swap! sequencer (fn [_] 3))

  @sequencer
  (play-seq 60)
  (stop-seq 14)

  (add-pattern 
   (scale-p
    bass-synth
    :C4 :major
    [[:2b> :5b] :05 [:2b> :5b] :|
     :04 [:2b> :5b]            :|
     :02 [:2b> :5b]            :|
     [:2b> :5b] :03 [:5b :2b>] :|
     [:5# :2b>] :05 [:5# :2b>] :|
     :04 [:2b> :5#]            :|
     :02 [:2b> :5#]            :|
     [:2b> :5#] :03 [:2b> :5#] :|
     [:2b> :6] :05 [:2b> :6]   :|
     :04 [:2b> :6]             :|
     :02 [:6 :2b>]             :|
     [:7 :2b>] :03 [:7 :2b>]   :|]
    1/8 0 [:attack 0.01 :amp 1.0 :release 1 :detune 3.0 :bwr 0.6 ]) :bass)

  (add-pattern 
   (phrase-p piano
             [(fn [d n] (choose (scale :c4 :major))) :| :|] 1/4 [:amp 0.3]) :drone)

  (remove-pattern :drone)
  (pause-seq)

  )
