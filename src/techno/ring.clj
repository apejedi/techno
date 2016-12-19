(ns techno.ring
  (:use [techno.sequencer :as s]
        [overtone.sc.trig])
  (:require [quil.core :as q]
            [quil.applet :as ap]
            )
  (:import [java.awt Color]))

(defonce sequencer (atom nil))
(defonce points (atom {}))
(defonce colors (atom {}))
(defonce ring-coords (atom {}))
(declare wheel)

(defn setup []
  ;(q/fullscreen)
  (q/background 0)
  (q/redraw)
;  (q/set-state! )
  (q/text-size 15)
;  (q/frame-rate 1)
  (q/no-loop)
  )


(defn gen-coords [x y init d player]
  (swap! points (fn [_] {}))
  (swap! colors (fn [_] {}))
  (let [data (s/get-sequencer-data player)
        size (:size data)
        step (s/get-st player)
        raw-size (+ 1 (/ (- size 1) step))
        theta (/ 6.28319 raw-size)
        h 0.84
        ratio 0.618033988749895
        markers (range 1 (+ size step) step)
        patterns (conj (into [] (s/get-p player)) [:legend {:data (zipmap markers (repeat (count markers) [:d]))}])
        gen-color (fn [circle]
                    (let [h (mod (+ h (* circle ratio)) 1)
                          color (Color/getHSBColor h 0.99 0.99)]
                      [(.getRed color) (.getGreen color) (.getBlue color)]
                      )
                    )
        gen-coord (fn [circle beat]
                    (let [angle (* -1 beat theta)
                          radius (+ init (* circle d))
                          a (- x (* radius (Math/sin angle)))
                          b (- y (* radius (Math/cos angle)))]
                      [a b]))
        coords (let [coord-map (transient {})]
                 (map
                  (fn [[k v] n]
                    (map
                     (fn [[o a]]
                       (let [offset (int (/ (- o 1) step))
                             pts (get coord-map offset {})]
                           (when (and (sequential? a)
                                      (not (nil? (first a))))
                             (println o)
                             (assoc! coord-map
                                     k (assoc pts offset (gen-coord n offset)))
                             ))
                        v)
                     )
                    (assoc! coord-map
                            k (assoc (get coord-map k) :color (gen-color n)))
                    )
                  patterns
                  (range 1 (inc (count patterns))))
                 (persistent! coord-map)
                 )
;        patterns (into [] (s/get-p player))
        ;; coords  (into
        ;;          {}
        ;;          (map
        ;;           (fn [[k v] n]
        ;;             (let [p (:data v)
        ;;                   p (if (map? p) (s/stretch-p p size) {})
        ;;                   h (mod (+ h (* n ratio)) 1)
        ;;                   color (Color/getHSBColor h 0.99 0.99)
        ;;                   rgb [(.getRed color) (.getGreen color) (.getBlue color)]
        ;;                   offsets
        ;;                   (filter
        ;;                    #(not (nil? %))
        ;;                    (map (fn [[o a]]
        ;;                           (if (and (sequential? a)
        ;;                                    (not (nil? (first a))))
        ;;                             (let [offset (int (/ (- o 1) step))
        ;;                                   angle (* -1 offset theta)
        ;;                                   radius (+ init (* (if (= k :legend) (inc n) n) d))
        ;;                                   a (- x (* radius (Math/sin angle)))
        ;;                                   b (- y (* radius (Math/cos angle)))
        ;;                                   pts (get @points offset [])]
        ;;                               (swap! points
        ;;                                      (fn [p] (assoc p offset
        ;;                                                    (conj pts [a b (if (= k :legend) o rgb)]))))
        ;;                               [a b (if (= k :legend) o rgb)])
        ;;                             nil)) p))]
        ;;               (swap! ring-coords (fn [r] (assoc r n offsets)))
        ;;               (swap! colors (fn [c] (assoc c k rgb)))
        ;;               (vector k offsets)))
        ;;           patterns
        ;;           (range 0 (count patterns))
        ;;           ))
        ]
    coords
    )
  )

(defn draw-state []
  (ap/with-applet wheel
    (q/redraw)
    )
  )

(defn draw-cursor []
  (let [g (.getGraphics wheel)
        pos (q/state :cursor)
        r (q/state :r)]
    (.beginDraw g)
    (ap/with-applet wheel
      (let [display (q/state :display-cursor)]
        (when display
          (q/fill 255 255 255)
          (doseq [[x y] (get @ring-coords pos)]
            (q/ellipse x y r r)
            )
          ))
      )
      (.endDraw g))
  )

(defn handle-key []
  (ap/with-applet wheel
    (let [cursor (q/state :cursor)
          cnt (count (keys (s/get-p @sequencer)))
          state (q/state-atom)
          key (q/key-as-keyword)
          new (+ cursor (cond (= key :up) 1
                              (= key :down) -1
                              true 0))
          display-cursor (q/state :display-cursor)]
      (swap!
       state
       (fn [s]
         (cond
           (and (or (= :up key) (= :down key)) (>= new 0) (< new cnt))
           (assoc s :cursor new)
           (= 10 (q/key-code))
           (assoc s :display-cursor (not display-cursor))
           true s)))
      (draw-cursor)
      )
    )
  )

(defn draw-line [beat]
  (let [g (.getGraphics wheel)]
    (.beginDraw g)
    (ap/with-applet wheel
      (let [r (q/state :r)
            step (s/get-st @sequencer)
            offset (dec (int beat))
            size (get (s/get-sequencer-data @sequencer) :size)
            raw-size (int (/ (- size 1) step))
            prev (if (= 0 offset) raw-size (dec offset))]
        (doseq [p (get @points prev)]
          (when (sequential? (last p))
              (apply q/fill (last p))
              (apply q/ellipse (conj (vec (take 2 p)) r r)))
          )

        (apply q/fill [255 255 0])
        (doseq [p (get @points offset)]
          (when (sequential? (last p))
              (apply q/ellipse (conj (vec (take 2 p)) r r))))
        ))
    (.endDraw g))
  )



(defn draw []
  (let [init (q/state :init)
        d (q/state :d)
        r (q/state :r)
        x (/ (q/width) 2)
        y (/ (q/height) 2)
        coords (gen-coords x y init d @sequencer)
        labels (keys (s/get-p @sequencer))]
    (doseq [[k v] coords]
      (doseq [[x y rgb] v]
        (when (sequential? rgb)
o          (apply q/fill rgb)
          (q/ellipse x y r r)
          )
        (when (not (sequential? rgb))
          (q/fill 255 255 255)
          (q/text (str rgb) x y)
          )
        )
      )

    (doall
     (map
      (fn [k y]
        (apply q/fill (get @colors k))
        ;(q/ellipse (- (q/width) 100) y r r)
        (q/text (str k) (- (q/width) 100 r 10) y)
        )
      labels
      (range (- (q/height) 40) (- (- (q/height) 40) (* 40 (inc (count labels)))) -40)
      ))
    nil
    )
  )


(defn ring [player & [init r d]]
  (let [r (if r r 2)
        d (if d d 4)
        uid (get (s/get-sequencer-data player) :uid)]
    (swap! sequencer (fn [_] player))
    (q/defsketch wheel
      :setup setup
      :draw draw
      :features [:present]
      :key-pressed handle-key
                                        ;:size [1500 800]
      :size :fullscreen
      )
    (ap/with-applet wheel
      (swap!
       (q/state-atom)
       (fn [s]
         (assoc (assoc (assoc (assoc s :d d) :r r) :init init) :cursor 0 :display-cursor false)
         ))
      )
    (on-latest-trigger
     player uid
     draw-line :draw-line)
    )
  )
