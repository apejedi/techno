(ns techno.programmor
  (:use [overtone.core]
        [overtone.osc.peer]
        [overtone.osc.util])
  )

(zero-conf-on)

(defonce server (atom (osc-server 4410 "techno")))
                                        ;(defonce client (atom (osc-client "192.168.1.13" 9000)))
(defonce client (atom (osc-client "10.251.243.105" 9000)))

(defonce server-client (atom (osc-client "127.0.0.1" 4410)))

(defonce state
  (atom
   {
    :cur-pattern nil
    :cur-step nil
    :cur-action nil
    :cur-synth nil
    :scope :pattern
    :patterns {}
    :synths {}
    :synth-params {}
    }))

(do
  (set! server (osc-server 4410 "techno"))
  (osc-listen
   @server
   (fn [m]
     (println m)
     )
   :debug)

  (osc-send @server-client
            "/test"
                 "test")
  )
