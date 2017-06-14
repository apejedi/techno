(ns techno.mpk
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

(def cur-synth (atom nil))

(def synth-params (atom {}))

(on-event
 [:midi :control-change]
 (fn [m]
   (let [note (:note m)
         inst-map {24 rise-pad 25 sweet 26 prophet 27 bpfsaw2 20 flute 21 overpad 22 bpfsaw 23 sin-inst}]
     (when (and (>= note 20) (<= note 27) (not (nil? (get inst-map note))))
       (reset! cur-synth (get inst-map note))
       )
     (when (and (>= note 1) (<= note 8))
       (let [params (into {}  (mapcat #(vector [(:name %) %]) (:params @cur-synth)))
             code {1 ["amp"] 2 ["dur" "t"] 3 ["attack" "atk"] 4 ["release" "decay"]}
             cur-params (get synth-params (:name @cur-synth) {})
             param-map (into {}
                             (mapcat #(vector [%
                                               (if (and (contains? code %) (some (fn [] contains? params ))))])
                                     (range 1 9))
                             )
             (map #(if (and (contains? code note) (some #(not (= -1 (.indexOf (get  note) (keys params)))) (vals code) ))
                               ) (range 1 9))
             param (if (and (contains? param-map note) (not (= -1 (.indexOf (get param-map note) (keys params)))))
                     (get params (first (get param-map note)))
                     (nth (:params @cur-synth) (reduce #(if ) 1 (vals param-map))))]
         )
       )
     )
   )
 :change-inst)

(on-event
 [:midi :program-change]
 (fn [m]
   (let [note (:note m)]
     (when (= note 24)
       ))
   )
 :change-inst2)
(on-event [:midi :note-on]
                (fn [m]
                  (let [chan (inc (:channel m))]
                                        ;(println (partition 2 (get ctr-map chan)))

                    (apply @cur-synth (get @synth-params (:name @cur-synth)))
                    ))
                ::prophet-midi)
