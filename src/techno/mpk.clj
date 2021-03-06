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

(def program (atom :pad))

(def drum-kit (atom {}))

;(event-debug-on)


(on-event
 [:midi :control-change]
 (fn [m]
   (let [note (:note m)
         control-map {:pad {24 rise-pad 25 sweet 26 prophet 27 bpfsaw2 20 flute 21 overpad2 22 bpfsaw 23 sin-inst}
                      :bass {24 acid-bass 25 bass2 26 plk-bass 27 wire-bass 20 bass-synth}
                      :inst {24 klang-test 25 reverb-test 26 piano 27 ks1 20 bing 21 bowed 22 rise-fall-pad2 23 cs80}}
         drum-map {24 {48 o-kick 49 o-snr 50 o-hat 51 o-clap 44 b-kick 45 b-snr 46 (drum-s [:Kit16-Electro] :c1) 47 (drum-s [:Kit16-Electro] :c2)}}
         drum-map (into drum-map (mapcat (fn [note kit]
                                           (let [sounds (group-samples (get drum-kits kit))]
                                             (vector [note
                                                      (into {}
                                                            (mapcat (fn [s n] (vector [n s])) (apply concat (map vals (vals (group-samples (get drum-kits kit))))) (range 48 84))
                                                              )
                                                        ;; (into {}
                                                        ;;       (mapcat (fn [s n] (vector [n s])) (vals (get drum-kits kit)) (range 48 73))
                                                        ;;       )
                                                        ])))
                                         [25 26 27 20 21 22 23
                                          32 33 34 35
                                          28 29 30 31] [:claves :KurzweilKit02 :KurzweilKit03 :KurzweilKit04 :KurzweilKit05 :KurzweilKit06 :KurzweilKit07
                                                        :Kit10-Vinyl :Kit11-Vinyl :Kit13-Acoustic :Kit8-Vinyl :Kit6-Electro :Kit9-Vinyl :Kit7-Electro :tabla]
                                         ))]
     (when (and (>= note 20) (<= note 27) (not (nil? (get-in control-map [@program note]))))
       (reset! cur-synth (get-in control-map [@program note]))
       )
     (when (= @program :drum)
       (reset! drum-kit (get drum-map note))
       )
     (when (and (>= note 1) (<= note 8))
       (let [params (into {}  (mapcat #(vector [(:name %) %]) (:params @cur-synth)))
             code {1 ["amp"] 2 ["dur" "t"] 3 ["attack" "atk"] 4 ["release" "decay"] 0 ["freq" "note"]}
             cur-params (get synth-params (:name @cur-synth) {})
             non-coded (filter #(= (.indexOf (flatten (map concat (vals code))) %) -1) (keys params))
             param-map (loop [m {} non-coded non-coded ctrl (range 1 9)]
                         (let [cur (first ctrl)
                               hard-coded (get code cur [])
                               d (first non-coded)
                               c (first (filter #(> (.indexOf hard-coded %) -1) (keys params)))
                               non-coded (if (not (nil? c)) non-coded (rest non-coded))
                               c (if (nil? c) d c)]
                           (if (> (count ctrl) 0)
                             (recur (assoc m cur c) non-coded (rest ctrl))
                             m)))
             param (get param-map note)
             param (if (not (nil? param)) (get params param))
             val (if (not (nil? param))
                   (scale-range (:data2 m) 1 128 0 (* (get param :default) 10)))]
         (when (not (nil? val))
           (swap! synth-params assoc-in [(:name @cur-synth) (:name param)] val))
         )
       )
     )
   )
 :change-inst)

(on-event
 [:midi :program-change]
 (fn [m]
   (let [note (:note m)
         program-map {4 :pad 5 :bass 6 :inst 7 :drum}]
     (when (not (nil? (get program-map note)))
       (reset! program (get program-map note)))
     )
   )
 :change-inst2)

(on-event [:midi :note-on]
          (fn [m]
                  (let [chan (inc (:channel m))
                        params (into {}  (mapcat #(vector [(:name %) %]) (:params @cur-synth)))
                        args (cond (contains? params "note") [:note (:data1 m)]
                                   (contains? params "freq") [:freq (midi->hz (:data1 m))]
                                   ;(contains? params "freq1") [:freq1 (midi->hz (:data1 m))]
                                    true [])
                        args #(vec (concat args (flatten (mapcat (fn [[k v]] [(keyword k) v]) (get @synth-params (:name %) [])))))
                        play (fn [synth args]
                               (techno.recorder/record-action [synth args])
                               (apply synth args)
                               ;; (when (= @program :drum)
                               ;;       (let [d (get @drum-kit (:data1 m))]
                               ;;         (when (not (nil? d))
                               ;;           (techno.recorder/record-action [d []])
                               ;;           (apply d [])))
                               ;;       )
                               ;; (when (not (= @program :drum))
                               ;;   (techno.recorder/record-action [@cur-synth args])
                               ;;   (apply @cur-synth args)
                               ;;   )
                               )]
                                        ;(println (partition 2 (get ctr-map chan)))
                    (when (not (nil? @cur-synth))
                      (if (sequential? @cur-synth)
                        (doseq [s @cur-synth]
                          (play s (args s)))
                        (play @cur-synth (args @cur-synth)))
                      )

                    ))
                ::prophet-midi)

;(remove-event-handler ::prophet-midi)
