(ns techno.ring
  (:use [techno.sequencer :as s]
        [overtone.sc.trig]
        [overtone.libs.event :only [on-event event]])
  (:require [quil.core :as q]
            [quil.applet :as ap]
            )
  (:import [java.awt Color]
           [javax.swing JComboBox JTextArea JFrame JTextField]
           [controlP5 ControlP5]
           (java.awt Dimension BorderLayout Font Color)
           (java.awt.event ActionListener ActionEvent)))


(defonce sequencer (atom nil))
(defonce points (atom {}))
(defonce colors (atom {}))
(defonce ring-coords (atom {}))
(declare wheel)

(defn setup []
  ;(q/fullscreen)
  (q/background 0)
;  (q/redraw)
;  (q/set-state! )
  (q/text-size 15)
;  (q/frame-rate 1)
 (q/no-loop)
  )


(defn get-coord [circle beat]
  (ap/with-applet wheel
    (let [init (q/state :init)
          d (q/state :d)
          step (s/get-st @sequencer)
          size (get (s/get-sequencer-data @sequencer) :size)
          raw-size (+ 1 (/ (- size 1) step))
          theta (/ 6.28319 raw-size)
          angle (* -1 beat theta)
          radius (+ init (* circle d))
          x (/ (q/width) 2)
          y (/ (q/height) 2)
          a (- x (* radius (Math/sin angle)))
          b (- y (* radius (Math/cos angle)))]
        [a b])
    )
  )

(defn gen-coords [x y init d player]
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
                 (doall
                  (map
                   (fn [[k v] n]
                     (let [data (get v :data)
                           data (if (map? data) data {1 []})
                           data (s/stretch-p data size)]
                         (doseq [[o a] data]
                           (let [offset (int (/ (- o 1) step))
                                 pts (get coord-map k {})]
                             (when (and (sequential? a)
                                        (not (nil? (first a))))
                               (assoc! coord-map
                                       k (assoc pts offset (gen-coord n offset)))
                               ))
                           ))
                     (assoc! coord-map
                             k (assoc (get coord-map k) :color
                                      (if (= k :legend)
                                        [255 255 255]
                                        (gen-color n))))
                     )
                   patterns
                   (range 1 (inc (count patterns)))))
                 (persistent! coord-map))]
    coords
    )
  )

(defn draw-state [& args]
  (ap/with-applet wheel
    (q/redraw)
    )
  )

(defn draw-action []
  (let [frame (JFrame. "Beat Action")
        text-box (JTextField. 20)
        kits (map name (keys techno.drums/drum-kits))
        kit (JComboBox. (into-array String kits))
        sounds (JComboBox. (into-array String (map name (keys (get techno.drums/drum-kits (keyword (first kits)))))))
        key-handler (fn [^String selected]
                      (.removeAllItems sounds)
                      (doseq [s (keys (get techno.drums/drum-kits (keyword selected)))]
                        (.addItem sounds (name s)))
                      )
        add-action (fn [^String selected]
                     )]
    (.setSize sounds (Dimension. 100 300))
    (.addActionListener kit
                        (reify ActionListener
                          (actionPerformed [this e]
                            (key-handler (.getSelectedItem (.getSource e)))
                            )
                          ))
    (doto (.getContentPane frame)
      (.add kit BorderLayout/WEST)
      (.add sounds BorderLayout/EAST)
      (.add text-box BorderLayout/SOUTH))
    (doto frame
      (.pack)
      (.setVisible true)
      )
    )
  )

(defn draw-cursor []
  (let [g (.getGraphics wheel)]
    (.beginDraw g)
    (ap/with-applet wheel
      (let [display (q/state :display-cursor)
            [circle beat] (q/state :cursor)
            [old-circle old-beat] (q/state :old-cursor)
            key (nth (keys @points) circle)
            old (nth (keys @points) old-circle)
            r (q/state :r)]
        (when display
          (q/fill 0 0 0)
          (apply q/ellipse (conj (apply get-coord (vector (inc old-circle) old-beat)) r r))
          (apply q/fill (get-in @points [old :color]))
          (doseq [[o [x y]] (get @points old)]
            (when (not (= :color o))
                (q/ellipse x y r r))
            )
          (q/fill 255 255 255)
          (doseq [[o [x y]] (get @points key)]
            (when (not (= :color o))
                (q/ellipse x y r r))
            )
          (q/fill 255 0 125)
          (apply q/ellipse (conj (apply get-coord (vector (inc circle) beat)) r r))
          )
        (when (not display)
          (q/fill 0 0 0)
          (apply q/ellipse (conj (apply get-coord (vector (inc circle) beat)) (+ r 5) (+ r 5)))
          (apply q/fill (get-in @points [key :color]))
          (doseq [[o [x y]] (get @points key)]
            (when (not (= :color o))
                (q/ellipse x y r r))
            )
          )
        ))
      (.endDraw g))
  )

(defn handle-key []
  (ap/with-applet wheel
    (let [[circle slot] (q/state :cursor)
          cnt (count (keys (s/get-p @sequencer)))
          step (s/get-st @sequencer)
          step (if step step 0.25)
          size (get (s/get-sequencer-data @sequencer) :size 0)
          raw-size (+ 1 (/ (- size 1) step))
          state (q/state-atom)
          key (q/key-as-keyword)
          new (vector (+ circle (cond (= key :up) 1
                                      (= key :down) -1
                                      true 0))
                      (+ slot (cond (= key :left) -1
                                      (= key :right) 1
                                      true 0)))
          display-cursor (q/state :display-cursor)]
      (when (or (= :left key) (= :right key) (= :up key) (= :down key) (= 10 (q/key-code)))
          (swap!
           state
           (fn [s]
             (cond
               (and display-cursor (or (= :left key) (= :right key) (= :up key) (= :down key))
                    (>= (first new) 0) (< (first new) cnt) (>= (second new) 0) (<= (second new) raw-size))
               (assoc (assoc s :cursor new) :old-cursor [circle slot])
               (= 10 (q/key-code)) (assoc s :display-cursor (not display-cursor))
               true s)))
          (draw-cursor))
      (when (and display-cursor (= :e key))

        )
      (when (= :e key)
        (draw-action)
        )
      ;; (when (= :w key)
      ;;   (let [cp5 (q/state :cp5)
      ;;         box (.getController cp5 "action")
      ;;         init (nil? box)
      ;;         box (if init (.addMultiList cp5 "action") box)
      ;;         ;[x y] (get-coord (inc circle) slot)
      ;;         ]
      ;;     (when init
      ;;       (.setPosition box 100 100)
      ;;       (.setSize box 500 500)
      ;;       ;; (.setItemHeight box 30)
      ;;       ;; (.setBarHeight  box 30)
      ;;       (.setColorActive box 1)
      ;;       (.add (.add (.add box "level1" 1) "level11" 11) "level111" 111)
      ;;       )
      ;;     (.show box)
      ;;     (draw-state)
      ;;     )
      ;;   )
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
            prev (if (= 0 offset) raw-size (dec offset))
            cursor (nth (keys @points) (first (q/state :cursor)))
            display-cursor (q/state :display-cursor)]
        (doseq [[k v] @points]
          (when (and (not (= :legend k)) (or (not display-cursor) (not (= k cursor))))
            (apply q/fill (get v :color))
            (when (contains? v prev)
              (apply q/ellipse (conj (vec (get v prev)) r r)))
            (apply q/fill [255 255 0])
            (when (contains? v offset)
              (apply q/ellipse (conj (vec (get v offset)) r r)))
            )
          )
        ))
    (.endDraw g))
  )



(defn draw []
  (let [init (q/state :init)
        d (q/state :d)
        r (q/state :r)
        x (/ (q/width) 2)
        y (/ (q/height) 2)
        step (s/get-st @sequencer)
        coords (gen-coords x y init d @sequencer)]
    (swap! points (fn [_] coords))
    (q/clear)
    (doseq [[k v] coords]
      (apply q/fill (get v :color))
      (doseq [[o [x y]] v]
        (when (and (not (= k :legend)) (number? o))
          (q/ellipse x y r r))
        (when (and (= :legend k) (number? o))
          (q/text (str (+ 1 (* o step))) x y))
        )
      )

    (doall
     (map
      (fn [[k v] y]
        (apply q/fill (get v :color))
        ;(q/ellipse (- (q/width) 100) y r r)
        (q/text (str k) (- (q/width) 100 r 10) y)
        )
      coords
      (range (- (q/height) 40) (- (- (q/height) 40) (* 40 (inc (count coords)))) -40)
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
         (merge s
                {:d d
                 :r r
                 :init init
                 :cursor [0 0]
                 :display-cursor false
                 :old-cursor [0 0]
       ;          :cp5 (ControlP5. wheel)
                 })
         ))
      ;(.setAutoDraw (q/state :cp5) true)
      )
    (on-latest-trigger
     player uid
     draw-line :draw-line)
    ;; (on-event ::pattern-added
    ;;  draw-state ::draw-state)
    )
  )
