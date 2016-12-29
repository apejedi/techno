(ns techno.ring
  (:use [techno.sequencer :as s]
        [techno.samples :only [drum-kits]]
        [overtone.sc.trig]
        [overtone.libs.event :only [on-event event]])
  (:require [quil.core :as q]
            [quil.applet :as ap]
            )
  (:import [java.awt Color]
           [javax.swing JComboBox JTextArea GroupLayout JFrame JTextField JButton]
;           [controlP5 ControlP5]
           (java.awt Dimension Font Color)
           (java.awt.event ActionListener ActionEvent)))


(defonce sequencer (atom nil))
(defonce points (atom {}))
(defonce colors (atom {}))
(defonce ring-coords (atom {}))
(declare wheel)

(defn find-in [coll x]
                  (some
                   (fn [[k v]]
                     (cond (= k x) [k]
                           (map? v) (if-let [r (find-in v x)]
                                      (into [k] r))))
                   coll))
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

(defn get-cur-action []
  (ap/with-applet wheel
    (let [[circle beat] (q/state :cursor)
          pattern (nth (keys @points) circle)
          step (s/get-st @sequencer)
          beat (+ 1 (* beat step))
          beat (if (= (mod beat (int beat)) 0.0) (int beat) beat)
          data (get (s/get-p @sequencer pattern) :data)
          action (if (map? data) (get data beat []) [])]
      action
      ))
  )

(defn get-action-str [action]
  (let [action (vec (map
                     #(cond (= overtone.sc.sample.PlayableSample (type %))
                            (list 'get-in 'drum-kits
                                  (find-in drum-kits (keyword
                                                      (clojure.string/replace (:name %) " " ""))))
                            (or (= (type %) overtone.studio.inst.Inst)
                                (= (type %) overtone.sc.synth.Synth))
                            (:name %)
                            true %)
                     action))
        action (vec (mapcat (fn [[a arg]]
                              (vector a arg "\n"))
                            (partition 2 action)))]
    (str "[" (apply str action) "]")))

(defn eval-action [action]
  (ap/with-applet wheel
    (let [[circle beat] (q/state :cursor)
          pattern (nth (keys @points) circle)
          step (s/get-st @sequencer)
          beat (+ 1 (* beat step))
          beat (if (= (mod beat (int beat)) 0.0) (int beat) beat)
          action (clojure.string/replace action "\n" "")]
      ;; (let [g (.getGraphics wheel)
;;             [circle slot] (q/state :cursor)]
;;         (.beginDraw g)
;;         (q/fill 0 0 0)
;;         (q/rect 100 50 300 100)
;;         (q/fill 255 255 255)
;;         (q/text (str " (import java.util.concurrent.ThreadLocalRandom) (use
;;         '[overtone.inst.synth]
;;         '[techno.core :as core]
;;         '[techno.sequencer :as s]
;;         '[techno.synths]
;;         '[techno.drum-patterns]
;;         '[techno.drums]
;;         '[techno.melody])
;; (s/set-action core/player
;; " pattern " " beat " " action ")") 100 100)
;;         (.endDraw g))

      (load-string
       (str " (import java.util.concurrent.ThreadLocalRandom) (use
'[overtone.core]
        '[overtone.inst.synth]
        '[techno.core :as core]
        '[techno.sequencer :as s]
        '[techno.synths]
        '[techno.drum-patterns]
        '[techno.drums]
        '[techno.samples]
        '[techno.melody])
(s/set-action core/player
" pattern " " beat " " action ")"))
      ))
  (draw-state)
  )


(defn draw-action []
  (let [frame (JFrame. "Beat Action")
        text-box (JTextArea.)
        kits (map name (keys drum-kits))
        kit (JComboBox. (into-array String kits))
        sounds (JComboBox. (into-array String (map name (keys (get drum-kits (keyword (first kits)))))))
        add (JButton. "Add")
        eval (JButton. "Eval")
        key-handler (fn [^String selected]
                      (.removeAllItems sounds)
                      (doseq [s (keys (get drum-kits (keyword selected)))]
                        (.addItem sounds (name s))))
        show-action (fn []
                      (.setText text-box (get-action-str (get-cur-action))))
        add-action (fn []
                     (ap/with-applet wheel
                       (let [[circle beat] (q/state :cursor)
                             pattern (nth (keys @points) circle)
                             step (s/get-st @sequencer)
                             beat (+ 1 (* beat step))
                             k (keyword (.getSelectedItem kit))
                             snd (keyword (.getSelectedItem sounds))
                             action (get-cur-action)
                             action-text (get-action-str (vec (conj action (get-in drum-kits (vector k snd)) [])))]
                         (.setText text-box action-text))))]
    (.setLayout frame nil)
    (ap/with-applet wheel
      (swap! (q/state-atom)
             (fn [s] (assoc s :action-text text-box))))
    (doto text-box
      (.setEditable true)
      (.setBackground Color/BLACK)
      (.setForeground Color/GREEN)
      (.setEditable true)
      (.setLineWrap true)
      (.setWrapStyleWord true)
      (.setFont (Font. "Monospaced" Font/PLAIN 14))
      (.setCaretColor Color/white))

    (.setBounds kit 10 10 150 20)
    (.setBounds sounds 160 10 150 20)
    (.setBounds add 330 10 60 20)
    (.setBounds eval 400 10 60 20)
    (.setBounds text-box 10 70 600 200)
    (.addActionListener kit
                        (reify ActionListener
                          (actionPerformed [this e]
                            (key-handler (.getSelectedItem (.getSource e)))
                            )))
    (.addActionListener sounds
                        (reify ActionListener
                          (actionPerformed [this e]
                            (show-action)
                            )))
    (.addActionListener add
                        (reify ActionListener
                          (actionPerformed [this e]
                            (add-action)
                            )))
    (.addActionListener eval
                        (reify ActionListener
                          (actionPerformed [this e]
                            (eval-action (.getText text-box))
                            )))
    (doto frame
      (.add kit)
      (.add sounds)
      (.add add)
      (.add eval)
      (.add text-box)
      (.setSize 700 300)
;      (.pack)
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
                      (cond (and (= key :right) (>= (inc slot) raw-size)) 0
                            (and (= key :left) (< (dec slot) 0)) (dec raw-size)
                        true (+ slot (cond (= key :left) -1
                                      (= key :right) 1
                                      true 0))))
          display-cursor (q/state :display-cursor)
          text-box (q/state :action-text)]
      (when (or (= :left key) (= :right key) (= :up key) (= :down key) (= 10 (q/key-code)))
        (swap!
         state
         (fn [s]
           (cond
             (and display-cursor (or (= :left key) (= :right key) (= :up key) (= :down key))
                  (>= (first new) 0) (<= (first new) cnt) (>= (second new) 0) (<= (second new) raw-size))
             (assoc (assoc s :cursor new) :old-cursor [circle slot])
             (= 10 (q/key-code)) (assoc s :display-cursor (not display-cursor))
             true s)))
        (when (and (not (nil? text-box)) (.isVisible text-box) (not (= 10 (q/key-code))))
          (.setText text-box (get-action-str (get-cur-action))))
        ;; (ap/with-applet wheel
        ;;   (let [g (.getGraphics wheel)
        ;;         [circle slot] (q/state :cursor)]
        ;;     (.beginDraw g)
        ;;     (q/fill 0 0 0)
        ;;     (q/rect 100 50 300 100)
        ;;     (q/fill 255 255 255)
        ;;     (q/text (str (nth (keys @points) circle) " " (+ 1 (* slot step)) " " slot " " raw-size) 100 100)
        ;;     (.endDraw g)))
        (draw-cursor))
      (when (= :e key)
        (draw-action)
        )
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
                 :action-text nil
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
