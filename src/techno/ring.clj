(ns techno.ring
  (:use [techno.sequencer :as s]
        [overtone.sc.trig])
  (:require [quil.core :as q]
            [quil.applet :as ap]
            ))

(defonce sequencer (atom nil))
(defonce window (atom nil))
(defonce points (atom {}))

(defn setup []
  ;(q/fullscreen)
  (q/background 0)
  (q/redraw)
  (q/set-state! :draw-line false :beat 1)
  (q/text-size 15)
;  (q/frame-rate 1)
  (q/no-loop)
  )


(defn gen-coords [x y init d player]
  (swap! points (fn [_] {}))
  (let [data (s/get-sequencer-data player)
        size (:size data)
        ;size 1.75
        ;step 0.25
        step (s/get-st player)
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
                                          radius (+ init (* n d))
                                          a (- x (* radius (Math/sin angle)))
                                          b (- y (* radius (Math/cos angle)))
                                          pts (get @points offset [])]
                                      (swap! points
                                             (fn [p] (assoc p offset (conj pts [a b]))))
                                      [a b])
                                    nil)) p))]
                      (vector k offsets)
                      ))
                  (s/get-p player)
                  (range 0 (count (s/get-p player)))
                  ))]
    coords
    )
  )

(defn draw-state []
  (ap/with-applet wheel
    (q/redraw)
    )
  )

(defn draw-line [beat]
;  (println beat)
  (ap/with-applet wheel
    (apply q/fill [255 255 255])
    (q/ellipse 500 500 50 50)
    ;; (swap! (q/state-atom) (fn [s] (assoc s :draw-line true)))
    ;; (swap! (q/state-atom) (fn [s] (assoc s :beat beat)))
    ;; (q/redraw)
    ;; (swap! (q/state-atom) (fn [s] (assoc s :draw-line false)))
    ;; (let [init 50
    ;;     d 20
    ;;       r 10
    ;;       beat (q/state :beat)
    ;;       x (/ (q/width) 2)
    ;;       y (/ (q/height) 2)
    ;;       step (s/get-st @sequencer)
    ;;       offset (int (/ (- beat 1) step))
    ;;       pts (get @points offset)]
    ;;   (apply q/fill [255 255 255])
    ;;   (doseq [p (get @points (dec offset))]
    ;;     (apply q/ellipse (conj p r r)))

    ;;   (apply q/fill [255 255 0])
    ;;   (doseq [p (get @points offset)]
    ;;     (apply q/ellipse (conj p r r)))
    ;;   )
    (q/redraw)
    )
  )



(defn draw []
  ;; (let [init 50
  ;;       d 20
  ;;       r 10]
  ;;     (cond (q/state :draw-line)
  ;;           (let [beat (q/state :beat)
  ;;                 x (/ (q/width) 2)
  ;;                 y (/ (q/height) 2)
  ;;                 step (s/get-st @sequencer)
  ;;                 offset (int (/ (- beat 1) step))
  ;;                 pts (get @points offset)]
  ;;             (apply q/fill [255 255 255])
  ;;             (doseq [p (get @points (dec offset))]
  ;;               (apply q/ellipse (conj p r r)))

  ;;             (apply q/fill [255 255 0])
  ;;             (doseq [p (get @points offset)]
  ;;               (apply q/ellipse (conj p r r)))
  ;;             )
  ;;           true (let [x (/ (q/width) 2)
  ;;                      y (/ (q/height) 2)
  ;;                      coords (gen-coords x y init d @sequencer)
  ;;                      colors (cycle [[255 255 255] [255 0 255]])
  ;;                      [[a b]] (first (vals coords))]
  ;;                  (doall
  ;;                   (map
  ;;                    (fn [[k v] n]
  ;;                      (apply q/fill (nth colors (rand-int 2)))
  ;;                      (doseq [[x y] v]
  ;;                                       ;(q/text (str o) (+ x 100) y)
  ;;                        (q/ellipse x y r r)
  ;;                        )
  ;;                      )
  ;;                    coords (range 0 (count coords)))))))
  )


(defn ring [player & [r d]]
  (let [r (if r r 2)
        d (if d d 4)
        uid (get (s/get-sequencer-data player) :uid)]
    (swap! sequencer (fn [_] player))
    (swap! window
           (fn [_]
             (q/defsketch wheel
               :setup setup
               :draw draw
                                        ;:features [:present]
               :size [1500 800]
               )))
    ;; (on-latest-trigger
    ;;  player uid
    ;;  draw-line :draw-line)
    ))
