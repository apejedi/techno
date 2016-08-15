(ns techno.controller
  (:use [overtone.core]
        [overtone.osc.peer]
        [overtone.osc.util]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.sketches])
  )

(zero-conf-on)

(defonce server (atom (osc-server 4410 "techno")))

(defonce queue? (atom false))

(defonce send-q (atom []))

(defonce sketch (atom {}))

(defonce client (atom (osc-client "192.168.0.19" 9000)))
(defonce server-client (atom (osc-client "127.0.0.1" 4410)))

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
  )
(comment
  (osc-send @client "/sketch/label1" "pattern1")
  (osc-listen @server (fn [msg] (println "Listener: " msg)) :debug)
  (osc-rm-all-listeners @server)
  (do
    (swap!
     sketch
     (fn [_]
       track1
       ))
    (load-handlers)
    )
  )
