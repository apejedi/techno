(ns techno.ring
  (:use [techno.sequencer :as s])
  (:require [quil.core :as q]
            [quil.applet :as ap]
            ))

(defonce sequencer (atom nil))
(defonce window (atom nil))


(defn setup []
  ;(q/fullscreen)
  (q/background 0)
  {}
  ;(q/no-loop)
  )

(defn gen-coords [x y init d player]
  (let [data (s/get-sequencer-data player)
        size (:size data)
        ;size 1.75
        ;step 0.25
        step (get-st player)
        raw-size (+ 1 (/ (- size 1) step))
        theta (/ 6.28319 raw-size)
        coords  (into
                 {}
                 (map
                  (fn [[k v] n]
                    (let [p (:data v)
                          p (if (map? p) p {})
                          offsets
                          (filter
                           #(not (nil? %))
                           (map (fn [[o a]]
                                  (if (and (sequential? a)
                                           (not (nil? (first a))))
                                    (let [offset (int (/ (- o 1) step))
                                          angle (* -1 offset theta)
                                          radius (+ init (* n d))]
                                      [(- x (* radius (Math/sin angle)))
                                       (- y (* radius (Math/cos angle)))])
                                    nil)) p))]
                      (vector k offsets)
                      ))
                  (s/get-p player)
                  (range 0 (count (s/get-p player)))
                  ))]
    coords
    )
  )
(defn draw [state]
  (let [x (/ (q/width) 2)
        y (/ (q/height) 2)
        init 50
        d 20
        r 10
        coords (gen-coords x y init d @sequencer)
        colors (cycle [[255 255 255] [255 0 255]])
        [[a b]] (first (vals coords))]
    (q/text-size 15)
    (q/text (str state) 0 1000)
    (doall
     (map
      (fn [[k v] n]
        (apply q/fill (nth colors n))
        (doseq [[x y] v]
          ;(q/text (str o) (+ x 100) y)
          (q/ellipse x y r r)
          )
        )
      coords (range 0 (count coords)))))
  )
(defn update [state]
  (assoc state :count (inc (get state :count 0)))
  )

(defn ring [player & [r d]]
  (let [r (if r r 2)
        d (if d d 4)]
    (swap! sequencer (fn [_] player))
    (q/defsketch wheel
      :setup setup
      :draw draw
      :features [:present]
      :update update
      :size :fullscreen
      )))
