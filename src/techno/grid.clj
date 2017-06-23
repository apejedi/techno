(ns techno.grid
  (:use [techno.samples :only [drum-kits]]
        [overtone.sc.trig]
        [overtone.sc.node]
        [overtone.music.pitch]
        [overtone.libs.event :only [on-event event]])
  (:require [techno.sequencer :as s]
            [quil.core :as q]
            [quil.applet :as ap]
            )
  (:import [java.awt Color]
           [javax.swing JComboBox JTextArea GroupLayout JFrame JTextField JButton]
;           [controlP5 ControlP5]
           (java.awt Dimension Font Color)
           (java.awt.event ActionListener ActionEvent)))


(def coord-map (atom {}))

(defn setup []
  (q/background 0)
  (q/text-size 15)
  (q/no-loop)
  (q/rect-mode :corners)
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
        coords (into {}
                     (mapcat (fn [r]
                               (mapcat (fn [b]
                                         (vector [(s/i-step (+ (* r bars) b))
                                                  (g-coords r
                                                            (int (mod (+ (inc (/ (mod b (int b)) step)) (* (/ 1 step) (dec (int b)))) (* (/ 1 step) bars))))]))
                                       (map #(+ %1 %2) (cycle (range 1 bars)) (flatten (repeat bars (range 0 1 step))))))
                             (range 0 (dec rows))))]
    (reset! coord-map coords)
    coords
    )
  )

(defn draw []
  (let [coords (vals @coord-map)]
    (doseq [[[x1 y1] [x2 y2]] coords]
      (q/rect x1 y1 x2 y2)
      )
    )
  )

;; (q/defsketch grid
;;   :setup setup
;;   :draw draw
;;   :size :fullscreen)

;; (mk-grid 2 3)
