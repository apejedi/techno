(ns techno.grid
  (:use [techno.samples :only [drum-kits]]
        [overtone.sc.trig]
        [overtone.sc.node]
        [overtone.music.pitch]
        [overtone.libs.event :only [on-event event]])
  (:require
   [techno.sequencer :as s]
   [quil.core :as q]
   [quil.applet :as ap]
   )
  (:import [java.awt Color]
           [javax.swing JComboBox JTextArea GroupLayout JFrame JTextField JButton]
                                        ;           [controlP5 ControlP5]
           (java.awt Dimension Font Color)
           (java.awt.event ActionListener ActionEvent)))


(declare grid)
(def state (atom {}))

(defn setup []
  (q/background 0)
  (q/text-size 15)
  (q/no-loop)
  ;(q/no-fill)
  (q/rect-mode :corners)
  )


(defn get-color [o]
  (let [p (get @state :pattern {})
        cur (get @state :cur 1)
        beat (get @state :beat)]
    (cond
      (= o beat) [255 69 0]
      (= o cur) [255 255 0]
      (and (contains? p o) (= o (apply max (keys p))))
      (if (and (contains? p o) (not (nil? (first (get p o)))))
        [255 0 255]
        [255 0 0])
      (and (contains? p o) (not (nil? (first (get p o))))) [124 252 0]
      true [220 220 220]))
  )

(defn draw []
  (let [cur (get @state :cur 1)
        actions (get @state :actions)]
    (doseq [[o [[x1 y1] [x2 y2]]] (get @state :coord-map)]
      (apply
       q/fill
       (get-color o))
      (q/rect x1 y1 x2 y2)
      (q/fill 0)
      (q/text (str o) (+ x1 10) (+ y1 15)))
    )
  )

(defn draw-beat [beat]
  ;; (swap! state assoc :beat (s/i-step (* (int beat) (get @state :step))))
                                        ;(q/redraw)
  (ap/with-applet grid
    (let [g (.getGraphics grid)
          step (get @state :step 0.25)
          size (get @state :size)
          beat (if (> beat size)
                 (nth (cycle (range 1 (inc size))) (dec beat))
                 beat)
          offset (s/i-step (inc (* (int beat) step)))
          cur (get-in @state [:coord-map offset])
          prev (get-in @state [:coord-map (s/i-step (- offset step))])]
      (.beginDraw g)
      ;; (q/fill 0 0 0 )
      ;; (q/rect 500 500 600 600)
      ;; (q/fill 255 255 255)
      ;; (q/text (str offset) 550 550)
      (when (not (nil? cur))
        (q/fill 255 120 0)
        (q/ellipse (- (first (second cur)) 30) (+ (second (second cur)) 10) 6 6)
        ;(apply q/rect (flatten cur))
        )
      (when (not (nil? prev))
        (q/fill 0 0 0)
        (q/ellipse (- (first (second prev)) 30) (+ (second (second prev)) 10) 6 6)
        ;(apply q/fill (get-color offset))
        ;(apply q/rect (flatten cur))
        )
      (.endDraw g)
      ))
  )

(defn handle-key []
  (let [key (q/key-as-keyword)
        cur (get @state :cur 1)
        coords (get @state :coord-map {})
        step (get @state :step 0.25)
        bars (get @state :bars 1)
        refresh (not (= -1 (.indexOf (concat [:left :right :up :down :x :a] (keys (get @state :actions {}))) key)))
        new (cond (= :left key) (- cur step)
                  (= :right key) (+ cur step)
                  (= :up key) (- cur bars)
                  (= :down key) (+ cur bars)
                  true cur)
        new (s/i-step new)
        new (if (contains? coords new) new cur)
        key-event @(:key-event (meta grid))]
    (when (contains? (get @state :actions) key)
      (let [c (get-in @state [:pattern cur] [])]
        (swap! state assoc-in [:pattern cur] (vec (concat c (get-in @state [:actions key]))))))

    (when refresh
      (swap! state assoc :cur new)
      (ap/with-applet grid
        (q/redraw)))
    )
  )

(defn set-state [keys val]
  (swap! state assoc-in keys val)
  )

(defn mk-grid [bars rows sequencer actions & [step space width height]]
  (let [step (if (nil? step) 0.25 step)
        space (if (nil? space) 20 space)
        width (if (nil? width) 50 width)
        height (if (nil? height) 30 height)
        g-coords (fn [r c]
                   [[(+ (* space (+ c 1)) (* width c)) (+ (* space (inc r)) (* height r))]
                    [(* (+ width space) (inc c)) (* (+ space height) (inc r))]])
        offsets (range 1 (* bars rows) step)
        coords (into {}
                     (mapcat
                      (fn [r]
                        (mapcat
                         (fn [c]
                           (let [a (g-coords r (inc c))
                                 o (s/i-step (+ (inc (* c step)) (* r bars)))]
                             [[o a]]))
                         (range 0 (* (/ 1 step) bars))))
                      (range 0 rows)))
        uid (get (s/get-sequencer-data sequencer) :uid)]
    (swap! state merge {:coord-map coords :rows rows :bars bars :step step :sequencer sequencer :size (* bars rows (/ 1 step)) :actions actions})
    (q/defsketch grid
      :setup setup
      :draw draw
      :size :fullscreen
      :key-pressed handle-key)
    (on-latest-trigger
     sequencer uid
     draw-beat :draw-beat)
    )
  )

;; (mk-grid 4 3)
