(ns techno.core
  (:use [overtone.core]
        [techno.sequencer]
        [techno.pattern]
        [techno.synths]
        )
  )

(defonce sequencer (atom nil))

;; (defmethod clojure.pprint/simple-dispatch overtone.sc.machinery.ugen.sc-ugen/SCUGen [ug]
;;   (println
;;    (str "#<sc-ugen: " (overtone-ugen-name (:name ug)) (:rate-name ug) " [" (count-ugen-args ug) "]>")))


(defn connect-sc
  ([] (connect-sc 57110))
  ([port] (connect-external-server port))
  )

(defn disconnect-sc []
  (overtone.sc.machinery.server.connection/shutdown-server)
  )

(defn play-seq [bpm]
  (swap! sequencer (fn [_] (get-seq bpm)))
  )

(defn pause-seq []
  (stop-seq @sequencer)
  )

(defn add-pattern [p key]
  (add-p @sequencer p key)
  )

(defn remove-pattern [key]
  (rm-p @sequencer key)
  )

(comment
  (def melody (phrase-p sweet [:c4] 1/4))
  )
