(ns techno.controller
  (:use [overtone.core]
        [overtone.osc.peer]
        [overtone.osc.util]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.sketches]
        [techno.synths]
        [techno.drums]
        [techno.samples])
  (:require [clojure.string :as string])
  )

(zero-conf-on)

(defonce server (atom (osc-server 4410 "techno")))

(defonce queue? (atom false))

(defonce send-q (atom {}))

(defonce sketch (atom {}))

(defonce synths (atom {}))

(defonce cur-pattern (atom {}))

(defonce p-map (atom {}))

(defonce sample1 (atom {}))

(defonce client (atom (osc-client "192.168.0.19" 9000)))
                                        ;(swap! client (fn [_] (osc-client "172.20.10.5" 9000)))

;(swap! client (fn [_] (osc-client "192.168.0.19" 9000)))
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
        arg-in (nth notes (dec n-in))
        arg (keyword (str arg-in octave))
        get-params (fn [arg]
                     (let [n (if (= note-arg :freq)
                               (midi->hz (note arg))
                               (note arg))
                           n (if note-arg [note-arg n] [n])]
                       (flatten (into n (get-in @synths [inst-in :param-vals] {})))
                       ))
        param-vals (get-params arg)
        scale (get-in @synths [inst-in :scale] false)]
    (if (> (first (:args msg)) 0)
      (if (and (not (= scale false))
               (get-in @synths [inst-in (keyword (str "chord" octave))] false))
        (let [scale-type (find-scale-name (map #(- (nth scale %) (nth scale (dec %)))
                                               (range 1 (count scale))))
              root (str (name (find-pitch-class-name (first scale))) octave)
              degree (inc (.indexOf (map find-pitch-class-name scale) (keyword arg-in)))
              degree (some #(if (= (val %) degree) (key %)) DEGREE)
              num-notes 3
              chord (chord-degree degree (keyword root) scale-type num-notes)]
          (doseq [c chord]
            (apply inst (get-params (find-note-name c)))
            )
          )
        (apply inst param-vals))
      (let [highlight (if scale
                        (map #(inc (.indexOf notes (name (find-pitch-class-name %))))
                             scale)
                        [])]
        (if (>= (.indexOf highlight n-in) 0)
          (osc-send @client (str "/synth" inst-in "/multipush1/" octave "/" n-in) 1))
        ))
  ))


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

(defn handle-samples
  ([samples] (handle-samples samples ["drum"]))
  ([samples path]
   (if (and (map? samples) (not (contains? samples :label)))
     (doseq [[k v] samples]
       (handle-samples v (conj path k)))
     (do
       (osc-send @client
                 (str "/" (string/join "/" (map str (butlast path))) "/label" (last path))
                 (if (map? samples)
                   (:label samples)
                   (str samples)))
       (osc-handle
        @server (str "/" (first path) "/multipush/" (string/join "/" (map str (rest path))))
        (fn [msg]
          (let [arg (first (:args msg))]
            (when (> arg 0)
              (if (map? samples)
                ((:value samples))
                (samples))
              ))
          )
        ))
     ))
  )


(defn load-handlers []
  (osc-rm-all-handlers @server)
  (doall
   (map (fn [[k v] num]
          (osc-send @client (str "/sketch/label" num) (name k))
          (swap! p-map (fn [m] (assoc m k num)))
          (osc-handle
           @server (str "/sketch/push" num)
           (fn [msg]
             (let [arg (first (:args msg))]
               (if (> arg 0)
                 (do
                   (osc-send @client (str "/sketch/push" num) 1)
                   (swap! cur-pattern (fn [_] k))
                   (doseq [i (range 1 25)]
                     (if (not (= i num))
                       (osc-send @client (str "/sketch/push" i) 0)
                       )
                     )
                   )
                 ))
             ))
          )
        @sketch (range 1 (inc (count (keys @sketch))))
        ))
  (osc-handle
   @server "/sketch/addp"
   (fn [msg]
     (let [arg (first (:args msg))]
       (if (> arg 0)
         (if (not @queue?)
           (if (keyword? @cur-pattern)
             (do
               (s/add-p @core/s-player (get @sketch @cur-pattern) @cur-pattern)
               (osc-send @client (str "/sketch/push" (get @p-map @cur-pattern) "/color") "green")
               ))
           ;(swap! send-q conj msg)
           (swap! send-q (fn [q] (assoc q :add (conj (get q :add []) @cur-pattern))))
           ))
       )))
  (osc-handle
   @server "/sketch/rmp"
   (fn [msg]
     (let [arg (first (:args msg))]
       (if (> arg 0)
         (if (not @queue?)
           (if (keyword? @cur-pattern)
             (do
               (s/rm-p @core/s-player @cur-pattern)
               (osc-send @client (str "/sketch/push" (get @p-map @cur-pattern) "/color") "blue")
               ))
           (swap! send-q (fn [q] (assoc q :rm (conj (get q :rm []) @cur-pattern))))
           ;(swap! send-q conj msg)
           )
         )
       )))
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
           (doseq [p (get @send-q :add)]
             (s/add-p @core/s-player (get @sketch p) p)
             (osc-send @client (str "/sketch/push" (get @p-map p) "/color") "green")
             )
           (doseq [p (get @send-q :rm)]
             (s/rm-p @core/s-player p)
             (osc-send @client (str "/sketch/push" (get @p-map p) "/color") "blue")
             )
           (swap! send-q (fn [_] {}))
           ;; (doseq [m @send-q]
           ;;   (peer-send-msg @server-client (apply mk-osc-msg (:path m) (osc-type-tag (:args m)) (:args m)))
           ;;   )
           ;; (swap! send-q (fn [_] []))
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
        (doseq [octave (range 1 8)]
          (osc-handle
           @server
           (str "/synth" k "/chord" octave)
           (fn [msg]
             (let [arg (first (:args msg))]
               (swap!
                synths
                (fn [synths]
                  (assoc-in synths [k (keyword (str "chord" octave))]
                            (> arg 0))
                  ))
               )))
          (doseq [n (range 1 13)]
            (osc-handle
             @server
             (str "/synth" k "/multipush1/" octave "/" n)
             call-synth)
            (if (>= (.indexOf highlight n) 0)
              (osc-send @client (str "/synth" k "/multipush1/" octave "/" n) 1)
              (osc-send @client (str "/synth" k "/multipush1/" octave "/" n) 0))
            )))
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
  (osc-send @client "/sketch/push1/color" "blue")
  (osc-send @client "/synth1/paramlabel1" "asd")
  (osc-send @client "/drum/10/label1" "test")
  (osc-listen @server (fn [msg] (println "Listener: " msg)) :debug)
  (osc-rm-all-listeners @server)
  (let [sc  (scale :C4 :minor)
        comp track2]
    (swap!
     sketch
     (fn [_]
       (zipmap (keys comp) (map #(atom %) (vals comp)))
       ))
    (swap!
     synths
     (fn [_]
       {1 {:synth bass-synth
           :scale sc}
        2 {:synth overpad
           :scale sc}
        3 {:synth ks1
           :scale sc}
        4 {:synth piano
           :scale sc}}))
    (load-handlers)
    )
  (s/rm-p @core/s-player :main3)
  (s/set-sp @core/s-player 2)
  (s/set-amp @core/s-player :sdst 0.5)
  )

    ;; (let [kit :Kit3-Acoustic
    ;;       samples (group-samples (kit drum-kits))
    ;;       clear-labels #(for [row (range 10 0 -1) col (range 1 13)]
    ;;                      (doall
    ;;                       (osc-send @client (str "/drum/" row "/label" col) "")))]
    ;;   (clear-labels)
    ;;   (doall
    ;;    (map (fn [row [group sounds]]
    ;;           (map
    ;;            (fn [col [k sound]]
    ;;              (let [num (last (re-seq #"\d+" (name k)))
    ;;                    group (if (nil? group)
    ;;                            (last (butlast (re-seq #"[A-Za-z]+" (name k))))
    ;;                            group)]
    ;;                (handle-samples
    ;;                 {row {col {:value sound :label (str (name group) num)}}})
    ;;                )
    ;;              )
    ;;            (range 1 (inc (count sounds)))
    ;;            sounds)
    ;;           )
    ;;         (range 10 (- 10 (inc (count samples))) -1)
    ;;         samples))
    ;;     )
