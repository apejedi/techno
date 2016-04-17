(ns techno.recorder
  (:use [overtone.core]
        [overtone.inst.drum])
  (:import (javax.swing JTextArea JFrame JTextField JScrollPane)
           (java.awt Dimension BorderLayout Font Color)
           (java.awt.event KeyListener KeyEvent))
  )


(defonce position (atom 0))
(defonce insts (atom (cycle [dance-kick noise-snare tone-snare])))
(defonce time-patterns (atom {}))
(defonce time-pattern (atom []))
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



(defn- crawl [graph f]
  (doseq [[k v] graph]
    (if (map? v)
      (crawl v f)
      (f v)
      )
    )
  )
