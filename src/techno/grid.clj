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


(defn get-offset [o & [ignore-cur]]
  (let [p (get @state :pattern {})
        cur (get @state :cur 1)
        size (if (> (count (keys (get @state :pattern))) 0)
               (apply max (keys (get @state :pattern)))
               1)
        step (get @state :step)
        o (if (and (or ignore-cur (not (= o cur))) (> o size))
            (s/i-step
             (nth (cycle
                   (range 1 (+ size step) step))
                  (int (/ (dec o) step))))
            o)]
    o
    )
  )

(defn get-color [o]
  (let [orig o
        p (get @state :pattern {})
        cur (get @state :cur 1)
        o (get-offset o)
        ;g (.getGraphics grid)
        ]
    ;; (.beginDraw g)
    ;; (q/fill 0 0 0 )
    ;; (q/rect 500 500 600 600)
    ;; (q/fill 255 255 255)
    ;; (q/text (str orig " " o) 550 550)
    ;; (.endDraw g)
    (cond
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
        actions (get @state :actions {})
        [[p1 q1] [r1 s1]]
        (get-in @state [:coord-map (apply max (keys (get @state :coord-map)))] [[1 1] [1 1]])]
    (doseq [[o [[x1 y1] [x2 y2]]] (get @state :coord-map)]
      (apply
       q/fill
       (get-color o))
      (q/rect x1 y1 x2 y2)
      (q/fill 0)
      (q/text (str o) (+ x1 10) (+ y1 15)))
    (doall
     (let [rows 10
           cols 5
           space 20
           width 50
           height 30
           g-coords (fn [r c]
                      [[(+ (* space (+ c 1)) (* width c)) (+ (* space (inc r)) (* height r))]
                       [(* (+ width space) (inc c)) (* (+ space height) (inc r))]])
           ys (range (+ s1 20) (+ s1 (* (count actions) 20)) 20)]
       (q/fill 0 0 0)
       (q/rect 10 s1 1400 (+ (last ys) 30))
       (q/fill 255 255 255)
       (map
        (fn [row]
          )
        (range 0 (int (Math/ceil (/ (count actions) cols)))))
       ;; (q/text (s/get-action-str (get-in @state [:pattern (get-offset cur true)])) 700 (+ s1 30))
       ;; (map
       ;;  (fn [[k v] y] (q/text (str (name k) ": " (s/get-action-str v)) 10 y))
       ;;  actions ys
       ;;  )
       ))
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
  (when (.hasFocus (.getNative (.getSurface grid)))
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
            new (s/i-step (if (> (int new) 0) new cur))
            new (if (contains? coords new) new cur)
            key-event @(:key-event (meta grid))]
        (when (contains? (get @state :actions) key)
          (let [c (get-in @state [:pattern cur] [])]
            (swap! state assoc-in [:pattern cur]
                   (vec (concat c (get-in @state [:actions key]))))))
        (when (= : key)
          (swap! state assoc :copy (get-in @state [:pattern cur]))
          (swap! state assoc-in [:pattern cur] []))
        (when (= : key)  (not (nil? (get @state :sequencer)))
              (s/add-p (get @state :sequencer) (get @state :pattern) :grid))
        (when (= : key)
          (swap! state assoc :pattern (s/stretch-p (get @state :pattern) new)))
        (when (and (.isShiftDown key-event) (not (= cur new)))
          (swap! state assoc-in [:pattern new] (get-in @state [:pattern cur]))
          (swap! state assoc-in [:pattern cur] nil))
        (when (= : key)
          (swap! state assoc :copy (get-in @state [:pattern cur])))
        (when (= : key)
          (swap! state assoc-in [:pattern cur] (get @state :copy)))
        (when refresh
          (swap! state assoc :cur new)
          (ap/with-applet grid
            (q/redraw)))
        ))
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
        uid (get (s/get-sequencer-data sequencer) :uid)
        actions (if (not (map? actions)) (into {} actions) actions)]
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
