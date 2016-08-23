(ns techno.controller
  (:use [overtone.core]
        [overtone.osc.peer]
        [overtone.osc.util]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.sketches]
        [techno.synths])
  (:require [clojure.string :as string])
  )

(zero-conf-on)

(defonce server (atom (osc-server 4410 "techno")))

(defonce queue? (atom false))

(defonce send-q (atom []))

(defonce sketch (atom {}))

(defonce synths (atom {}))

(defonce client (atom (osc-client "192.168.0.19" 9000)))
(defonce server-client (atom (osc-client "127.0.0.1" 4410)))

(defn call-synth [msg]
  (let [[inst-in push octave n-in] (re-seq #"[\d]+" (:path msg))
        inst-in (Integer/parseInt inst-in)
        n-in (Integer/parseInt n-in)
        inst (get-in @synths [inst-in :synth])
        note-arg (if (or (instance? overtone.studio.inst.Inst inst)
                         (instance? overtone.sc.synth.Synth inst))
                   (cond (some #(= (:name %) "freq") (:params inst)) :freq
                         (some #(= (:name %) "note") (:params inst)) :note
                         true false))
        notes ["C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B"]
        arg (nth notes (dec n-in))
        arg (keyword (str arg octave))
        n (if (= note-arg :freq)
            (midi->hz (note arg))
            (note arg))
        n (if note-arg [note-arg n] [n])
        param-vals (flatten (into n (get-in @synths [inst-in :param-vals] {})))]
    (if (> (first (:args msg)) 0)
      (apply inst param-vals)
      (let [highlight (if (contains? (get @synths inst-in) :scale)
                        (map #(inc (.indexOf notes (name (find-pitch-class-name %))))
                             (:scale (get @synths inst-in)))
                        [])]
        (if (>= (.indexOf highlight n-in) 0)
          (osc-send @client (str "/synth" inst-in "/multipush1/" octave "/" n-in) 1))
        ))
    )
  )

(defn mod-synth [msg]
  (let [[inst-in param-in] (re-seq #"[\d]+" (:path msg))
        inst-in (Integer/parseInt inst-in)
        inst (get-in @synths [inst-in :synth])
        params (:params inst)
        param-vals (get-in @synths [inst-in :param-vals] {})
        param (nth params (dec (Integer/parseInt param-in)))
        p (:name param)
        val (first (:args msg))
        allowed ["amp" "decay" "attack" "release" "atk" "t" "dur" "coef"]]
    (if (> (.indexOf allowed p) -1)
      (swap! synths
             (fn [synths]
               (assoc-in synths [inst-in :param-vals]
                         (assoc param-vals (keyword p)
                                (cond (= p "amp") val
                                      (= p "coef") val
                                      true (scale-range val 0 1 0.1 12)))))))
    )
  )

(defn load-handlers []
  (osc-rm-all-handlers @server)
  (doall
   (map (fn [[k v] num]
          (osc-send @client (str "/sketch/label" num) (name k))
          (osc-handle
           @server (str "/sketch/toggle" num)
           (fn [msg]
             (if (not @queue?)
               (let [arg (first (:args msg))]
                 (if (> arg 0)
                   (s/add-p @core/s-player (get @sketch k) k)
                   (s/rm-p @core/s-player k)
                   ))
               (swap! send-q conj msg)
               )))
          )
        @sketch (range 1 (inc (count (keys @sketch))))
        ))
  (osc-handle
   @server "/sketch/play"
   (fn [msg]
     (let [arg (first (:args msg))]
       (if (> arg 0)
         (core/start-player)
         (kill @core/s-player)
         )
       )))
  (osc-handle
   @server "/sketch/tempo"
   (fn [msg]
     (let [arg (first (:args msg))]
       (if (node-active? @core/s-player)
         (s/set-sp @core/s-player arg))
       )))
  (osc-handle
   @server "/sketch/queue"
   (fn [msg]
     (let [arg (first (:args msg))]
       (if (> arg 0)
         (swap! queue? (fn [_] true))
         (do
           (doseq [m @send-q]
             (peer-send-msg @server-client (apply mk-osc-msg (:path m) (osc-type-tag (:args m)) (:args m)))
             )
           (swap! send-q (fn [_] []))
           (swap! queue? (fn [_] false)))
         ))))
  (if (not (nil? @synths))
    (doseq [[k s] @synths]
      (osc-send @client (str "/synth" k "/synthlabel") (get-in s [:synth :name]))
      (let [notes ["C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B"]
            highlight (if (contains? s :scale)
                        (map #(inc (.indexOf notes (name (find-pitch-class-name %))))
                             (:scale s))
                        [])]
        (doseq [n (range 1 13) octave (range 1 8)]
          (osc-handle
           @server
           (str "/synth" k "/multipush1/" octave "/" n)
           call-synth)
          (if (>= (.indexOf highlight n) 0)
            (osc-send @client (str "/synth" k "/multipush1/" octave "/" n) 1)
            (osc-send @client (str "/synth" k "/multipush1/" octave "/" n) 0))
          ))
      (let [params (get-in s [:synth :params])]
        (doall
         (map (fn [n p]
                (osc-send @client (str "/synth" k "/paramlabel" n) (:name p))
                (osc-handle
                 @server
                 (str "/synth" k "/param" n)
                 mod-synth))
              (range 1 (inc (count params)))
              params
              ))
        )
      ))
  )
(comment
  (osc-send @client "/sketch/label1" "pattern1")
  (osc-send @client "/synth1/paramlabel1" "asd")
  (osc-listen @server (fn [msg] (println "Listener: " msg)) :debug)
  (osc-rm-all-listeners @server)
  (let [sc  (scale :C4 :major)]
    (swap!
     sketch
     (fn [_]
        strings
        ))
    (swap!
     synths
     (fn [_]
       {1 {:synth bass-synth
           :scale sc
           }
        2 {:synth ks1
           :scale sc
           }
        3 {:synth piano
           :scale sc
           }
        }))
    (load-handlers)
    )
  )
