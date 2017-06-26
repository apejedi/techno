(ns techno.grid
  ;; (:use [techno.samples :only [drum-kits]]
  ;;       [overtone.sc.trig]
  ;;       [overtone.sc.node]
  ;;       [overtone.music.pitch]
  ;;       [overtone.libs.event :only [on-event event]])
  (:require
   ;[techno.sequencer :as s]
            [quil.core :as q]
            [quil.applet :as ap]
            )
  (:import [java.awt Color]
           [javax.swing JComboBox JTextArea GroupLayout JFrame JTextField JButton]
;           [controlP5 ControlP5]
           (java.awt Dimension Font Color)
           (java.awt.event ActionListener ActionEvent)))



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
        cur (get @state :cur 1)]
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
  (let [cur (get @state :cur 1)]
    (doseq [[o [[x1 y1] [x2 y2]]] (get @state :coord-map)]
      (apply
       q/fill
       (get-color o))
      (q/rect x1 y1 x2 y2)
      (q/fill 0)
      (q/text (str o) (+ x1 10) (+ y1 15))
      ))
  )

(defn handle-key []
  (let [key (q/key-as-keyword)
        cur (get @state :cur 1)
        coords (get @state :coord-map {})
        step (get @state :step 0.25)
        bars (get @state :bars 1)
        i-step #(if (= (mod % (int %)) 0.0) (int %) %)
        new (cond (= :left key) (- cur step)
                  (= :right key) (+ cur step)
                  (= :up key) (- cur bars)
                  (= :down key) (+ cur bars)
                  true cur)
        new (i-step new)
        new (if (contains? coords new) new cur)]
    (swap! state assoc :cur new)
    (ap/with-applet grid
      (q/redraw))
    )
  )

(defn mk-grid [bars rows & [step space width height]]
  (let [step (if (nil? step) 0.25 step)
        space (if (nil? space) 10 space)
        width (if (nil? width) 50 width)
        height (if (nil? height) 30 height)
        g-coords (fn [r c]
                   [[(+ (* space (+ c 1)) (* width c)) (+ (* space (inc r)) (* height r))]
                    [(* (+ width space) (inc c)) (* (+ space height) (inc r))]])
        offsets (range 1 (* bars rows) step)
        i-step #(if (= (mod % (int %)) 0.0) (int %) %)
        coords (into {}
                     (mapcat
                      (fn [r]
                        (mapcat
                         (fn [c]
                           (let [a (g-coords r (inc c))
                                 o (i-step (+ (inc (* c step)) (* r bars)))]
                             [[o a]]))
                         (range 0 (* (/ 1 step) bars))))
                      (range 0 rows)))]
    (swap! state merge {:coord-map coords :rows rows :bars bars :step step})
    (q/defsketch grid
      :setup setup
      :draw draw
      :size :fullscreen
      :key-pressed handle-key)
    )
  )

;; (mk-grid 4 3)
