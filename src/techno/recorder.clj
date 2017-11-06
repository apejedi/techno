(ns techno.recorder
  (:use [overtone.core]
        [overtone.inst.drum]
                                        ;[overtone.at-at :only [every]]
        )
  (:import (javax.swing JTextArea JFrame JTextField JScrollPane)
           (java.awt Dimension BorderLayout Font Color)
           (java.awt.event KeyListener KeyEvent))

  (:require [techno.sequencer :as s]
            [techno.player :as p]
            [techno.core :as core]))


(defonce position (atom 0))
(defonce insts (atom (cycle [dance-kick noise-snare tone-snare])))
(defonce time-patterns (atom {}))
(defonce time-pattern (atom {}))
(defonce quantized-pattern (atom {}))
(def last-recorded (atom {}))
(defonce recording (atom false))


;(start-recorder [dance-kick noise-snare tone-snare])


(defn create-console [& handler]
  (let [frame (JFrame. "Sequence Recorder")
        output (JTextArea.)
        pane (JScrollPane. output)
        text-box (JTextField. 20)
        key-handler (if (not (nil? (first handler)))
                      (first handler)
                      (fn [^KeyEvent e ^JTextArea o ^JTextField t]
                        (let [key-pressed (if (= (.getID e) KeyEvent/KEY_TYPED)
                                            (str (.getKeyChar e))
                                            (KeyEvent/getKeyText (.getKeyCode e)))
                              mods (KeyEvent/getModifiersExText(.getModifiersEx e))]
                          (.append o (str "pressed " key-pressed
                                          " mods: " mods
                                          "\n"))
                          (.setText t ""))
                        )
                      )]
    (doto output
      (.setBackground Color/BLACK)
      (.setForeground Color/GREEN)
      (.setEditable false)
      (.setFont (Font. "Monospaced" Font/PLAIN 14))
      )
    (.addKeyListener text-box (reify KeyListener
                                (keyPressed [this e])
                                (keyReleased [this e])
                                (keyTyped [this e]
                                  (key-handler e output text-box)
                                  )
                                ))
    (doto pane
      (.setPreferredSize (Dimension. 800 500))
      )
    (doto (.getContentPane frame)
      (.add pane BorderLayout/CENTER)
      (.add text-box BorderLayout/SOUTH))
    (doto frame
      (.pack)
      (.setVisible true)
      )
    )
  )

(defn start-recorder [instruments]
  (swap! insts (fn [_] (cycle instruments)))
  (create-console (fn [^KeyEvent e ^JTextArea out ^JTextField text]
                    (let [key-pressed (keyword (if (= (.getID e) KeyEvent/KEY_TYPED)
                                                 (str (.getKeyChar e))
                                                 (KeyEvent/getKeyText (.getKeyCode e))))
                          mods (KeyEvent/getModifiersExText (.getModifiersEx e))
                          key-seq [:q :w :e :r :t :y :u :i :o :p :a :s :d :f :g :h :j :k :l :z :x :c :v :b :n :m]
                          pos (if (= (keyword ",") key-pressed) (+ 26 @position) @position)
                          get-inst (fn [key-pressed]
                                     (nth @insts
                                          (if (>= (.indexOf key-seq key-pressed) 0)
                                            (+ pos (.indexOf key-seq key-pressed))
                                            0)))
                          inst (get-inst key-pressed)
                          get-display (fn [inst] (if (and (coll? inst) (contains? inst :name))
                                                  (.name inst)
                                                  inst))]
                      (if (and (= key-pressed :l) (= mods "Alt"))
                        (do (.append out "\nListing instruments\n")
                            (doseq [k key-seq]
                              (.append out (str k " " (get-display (get-inst k)) "\n"))))
                        (do (.append out (str (get-display inst) "\n\n"))
                            (if (not (nil? inst)) (inst)))
                        )
                      (.setText text "")
                      (swap! position (fn [_] pos))
                      )))

  )

  ;; (record-time-pattern)
  ;; (s/play 1 @pulse-beat)
  ;; (def b1 (get-time-pattern))

(defn record-time-pattern []
  (create-console
   (fn [^KeyEvent e ^JTextArea out ^JTextField text]
     (let [timestamp (System/currentTimeMillis)
           key-pressed (keyword (if (= (.getID e) KeyEvent/KEY_TYPED)
                                  (str (.getKeyChar e))
                                  (KeyEvent/getKeyText (.getKeyCode e))))
           mods (KeyEvent/getModifiersExText (.getModifiersEx e))]
       (if (= key-pressed :r)
         (dosync (swap! recording (fn [r] (not r)))
             (if @recording
               (dosync (.append out "\nRecording\n")
                   (swap! time-pattern (fn [_] [timestamp])))
                 (.append out "\nStopped recording\n")))
         (dosync
          (swap! time-pattern (fn [t] (conj t timestamp)))
          (.append out (str "\n" timestamp "\n")))
         )
       (.setText text "")
       )
     ))
  )



;; Todo, use dur to scale offsets in sequence

(defn get-time-pattern []
  @time-pattern
  )

(defn- get-dur [p]
  "Calculates the duration in seconds of a pattern recorded by record-time-pattern"
  (let [start (first @time-pattern)
        end (last @time-pattern)
        dur (/ (- end start) 1000)]
    dur
    )
  )

(defn pp-time []
  (println @time-pattern)
  )


(defn- test-instrument [instruments]
  (loop [position 0 insts (cycle instruments)]
    (let [key-pressed (read-line)
          pos (if (.equals "," key-pressed) (+ 26 position) position)
          index (+ pos (Math/abs (- 97 (int (.charAt key-pressed 0)))))
          inst (nth insts index)
          display (if (contains? inst :name)
                    (.name inst)
                    inst
                    )
          ]
      (println display " current position: " pos)
      (if (not (nil? inst)) (inst))
      (if (.equals "." key-pressed)
        nil
        (recur pos insts)
        )
      ))
  )

(defsynth rec-out [buf 0]
  (record-buf (in:ar 0 2) buf :action 2 :loop 0)
  )
(defn record-out [path dur]
  (let [buf (buffer (* 44100 dur) 2)
        rec (rec-out [:tail 1] buf)]
    (on-node-destroyed rec
                       (fn [_]
                         (buffer-save buf path)
                         (buffer-free buf)))
    )
  )

                                        ;(start-record-pattern)
(defn start-record-pattern []
  (reset! time-pattern (java.util.LinkedList.))
  (reset! recording true)
  )

(defn stop-record-pattern []
  (reset! recording false)
  )

(defn record-action [action]
  (let [t (System/nanoTime)]
    (when @recording
        (.add @time-pattern [t action]))
    )
  )

(defn- crawl [graph f]
  (doseq [[k v] graph]
    (if (map? v)
      (crawl v f)
      (f v)
      )
    )
  )

(defn play-time-pattern []
  (let [start (first (first @time-pattern))
        offsets (map #(float (/ (- (first %) start) 1000000)) @time-pattern)
        n (+ (now) 1000)]
    (doseq [i (range 0 (count @time-pattern))]
      (at (+ n (nth offsets i))
          (apply (first (second (.get @time-pattern i))) (second (second (.get @time-pattern i))))
         )
      )
    )
  )

(defn quantize-time-pattern
  ([] (quantize-time-pattern (* (s/get-sp core/player) 60) (s/get-st core/player)))
  ([tempo step]
   (let [quant (float (* step (/ 60 tempo))) ;;duration of step
         begin (first (first @time-pattern))
         p (reduce (fn [p [o a]]
                     (let [o (Math/floor (/ (- o begin) quant 1000000000))
                           o (inc (* o step))
                           o (if (= (mod o (int o)) 0.0) (int o) o)]
                       (if (contains? p o) (assoc p o (vec (concat (get p o) a)))
                           (assoc p o a))
                       ))
                   {}
                   @time-pattern)]
     p
     ))
  )

(defn mk-map-p
  ([] (mk-map-p (p/get-state core/player :bpm) (p/get-state core/player :div)))
  ([bpm div]
   (let [quant (float (/ 60 bpm div)) ;;duration of step
         begin (first (first @time-pattern))
         pat (reduce (fn [pat [o a]]
                       (let [o (inc (int (Math/floor (/ (- o begin) quant 1000000000))))
                             pos (p/get-pos o div)
                             c (get-in pat pos [])
                             a (vec (concat c a))]
                         (assoc-in pat pos a)
                         ))
                     {:div div}
                     @time-pattern)]
     pat
     ))
  )

(defn get-tempo [step]
  (let [start (first (first @time-pattern))
        limit 99903328
        shortest (reduce
                  (fn [m n]
                    (let [prev (first (.get @time-pattern (dec n)))
                          cur (first (.get @time-pattern n))]
                      (if (>= (- cur prev) limit)
                        (min m (- cur prev))
                        m)
                      )
                    )
                  1000000000 (range 1 (count @time-pattern)))]
    (int (* (/ 1 (/ shortest 1000000000 step)) 60))
    )
  )
