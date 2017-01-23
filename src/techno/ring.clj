(ns techno.ring
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

(defonce sequencer (atom nil))
(defonce points (atom {}))
(defonce colors (atom {}))
(defonce ring-coords (atom {}))
(declare wheel)

(defprotocol Item
  (toString [this])
  (getValue [this]))

(deftype SampleItem [state]
  Item
  (toString [this] (:display state))
  (getValue [this] (:value state)))

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
                           data-size (get v :size (apply max (keys data)))
                           data (apply dissoc data
                                       (filter #(> % data-size) (keys data)))
                           pattern-size (/ (dec data-size) step)
                           data (s/stretch-p data size)]
                         (doseq [[o a] data]
                           (let [offset (int (/ (- o 1) step))
                                 pts (get coord-map k {})]
                             (when (and (sequential? a)
                                        (not (nil? (first a))))
                               (assoc! coord-map
                                       k (assoc pts offset (gen-coord n offset)))
                               )))
                         (assoc! coord-map k
                                 (assoc (get coord-map k {}) :bound
                                        (gen-coord n pattern-size)))
                         )
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

(defn get-cur-action [& [circle beat]]
  (ap/with-applet wheel
    (let [[circle beat] (if (not (nil? circle)) [circle beat] (q/state :cursor))
          pattern (nth (keys @points) circle)
          step (s/get-st @sequencer)
          size (get (s/get-sequencer-data @sequencer) :size)
          beat (+ 1 (* beat step))
          beat (if (= (mod beat (int beat)) 0.0) (int beat) beat)
          data (get (s/get-p @sequencer pattern) :data)
          action (if (map? data) (get (s/stretch-p data size) beat []) [])]
      action
      ))
  )

(defn get-action-str [action ]
  (s/get-action-str action drum-kits "drum-kits"))


(defn eval-action [action]
  (ap/with-applet wheel
    (let [[circle beat] (q/state :cursor)
          pattern (nth (keys @points) circle)
          step (s/get-st @sequencer)
          beat (+ 1 (* beat step))
          beat (if (= (mod beat (int beat)) 0.0) (int beat) beat)
          action (clojure.string/replace action "\n" "")
          body (get (s/get-p @sequencer pattern) :body {})]
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
" pattern " " beat " " action " true)"))
      (s/mod-p @sequencer pattern :body (assoc body beat action))
      ))
  (draw-state)
  )


(defn draw-action []
  (let [frame (JFrame. "Beat Action")
        text-box (JTextArea.)
        kits (sort (map name (keys drum-kits)))
        kit (JComboBox. (into-array String kits))
        ;; sounds (JComboBox. (into-array String (sort (map name (keys (get drum-kits (keyword (first kits))))))))
        sounds (JComboBox. (into-array
                            SampleItem
                            (map
                             #(SampleItem. {:display % :value (vector (keyword (first kits)) (keyword %))})
                             (sort (map name (keys (get drum-kits (keyword (first kits)))))))))
        add (JButton. "Add")
        eval (JButton. "Eval")
        play (JButton. "Play")
        filter-box (JTextField.)
        pos (JTextField.)
        templates (JComboBox. (into-array String ["chord" "rand"]))
        key-handler (fn [^String selected]
                      (.removeAllItems sounds)
                      (doseq [s (keys (get drum-kits (keyword selected)))]
                        (.addItem sounds
                                  (SampleItem. {:display (name s) :value (vector (keyword selected) s)})
                                  ;(name s)
                                  )))
        show-action (fn []
                      (.setText text-box (get-action-str (get-cur-action))))
        add-action (fn []
                     (ap/with-applet wheel
                       (let [[circle beat] (q/state :cursor)
                             pattern (nth (keys @points) circle)
                             step (s/get-st @sequencer)
                             beat (+ 1 (* beat step))
                             k (keyword (.getSelectedItem kit))
                             snd (.getSelectedItem sounds)
                             ;snd (keyword (.getSelectedItem sounds))
                             action (get-cur-action)
                             action-text (get-action-str (vec (conj action (get-in drum-kits (.getValue snd)) [])))
                             ;action-text (get-action-str (vec (conj action (get-in drum-kits (vector k snd)) [])))
                             ]
                         (.setText text-box action-text))))]
    (.setLayout frame nil)
    (ap/with-applet wheel
      (swap! (q/state-atom)
             (fn [s] (assoc (assoc s :action-text text-box) :action-pos pos))))
    (doto text-box
      (.setBackground Color/BLACK)
      (.setForeground Color/GREEN)
      (.setEditable true)
      (.setLineWrap true)
      (.setWrapStyleWord true)
      (.setFont (Font. "Monospaced" Font/PLAIN 14))
      (.setCaretColor Color/white))
    (doto pos
      (.setBackground Color/BLACK)
      (.setForeground Color/GREEN)
      (.setFont (Font. "Monospaced" Font/PLAIN 14)))

    (.setBounds kit 10 10 150 20)
    (.setBounds sounds 160 10 200 20)
    (.setBounds filter-box 10 35 150 20)
    (.setBounds play 160 35 100 20)
    (.setBounds add 380 10 60 20)
    (.setBounds eval 450 10 60 20)
    (.setBounds pos 520 10 160 20)
    (.setBounds templates 10 70 150 20)
    (.setBounds text-box 10 130 600 200)
    (.addActionListener kit
                        (reify ActionListener
                          (actionPerformed [this e]
                            (key-handler (.getSelectedItem (.getSource e)))
                            )))
    (.addActionListener sounds
                        (reify ActionListener
                          (actionPerformed [this e]
                            (let [snd (.getSelectedItem sounds)
                                  ]
                              (when (not (nil? snd))
                                  ((get-in drum-kits (.getValue snd))))
                              )
                            (show-action)
                            )))
    (.addActionListener add
                        (reify ActionListener
                          (actionPerformed [this e]
                            (add-action)
                            )))
    (.addActionListener filter-box
                        (reify ActionListener
                          (actionPerformed [this e]
                            (let [texts (clojure.string/split (.getText filter-box) #" ")]
                              (.removeAllItems sounds)
                              (doseq [[kit snds] drum-kits]
                                (doseq [snd (keys snds)]
                                  (when
                                      (every? #(or (.contains
                                                    (.toLowerCase (name kit))
                                                    (.toLowerCase %))
                                                   (.contains (.toLowerCase (name snd))
                                                              (.toLowerCase %)))
                                             texts)
                                      (.addItem sounds
                                                (SampleItem.
                                                 {:display (name snd)
                                                  :value (vector kit snd)})
                                                ))))
                              )
                            )))
    (.addActionListener play
                        (reify ActionListener
                          (actionPerformed [this e]
                            (let [snd (.getSelectedItem sounds)]
                              ((get-in drum-kits (.getValue snd)))
                              )
                            )))
    (.addActionListener eval
                        (reify ActionListener
                          (actionPerformed [this e]
                            (eval-action (.getText text-box))
                            )))
    (.addActionListener templates
                        (reify ActionListener
                          (actionPerformed [this e]
                            (let [selected (.getSelectedItem (.getSource e))]
                              (.setText
                               text-box
                               (cond (= selected "chord") "(s/chord-p\ninst\n (chord) [])"
                                     (= selected "rand") "(let [coll (atom (s/x-seq []))]\n
[(fn [] (reset! coll (rest @coll)) \n
(doseq [n (chord-degree (first @coll) :B3 :minor)] \n
(overpad :note n))\n
) []])"))
                              )
                            )))
    (doto frame
      (.add kit)
      (.add sounds)
      (.add add)
      (.add eval)
      (.add play)
      (.add filter-box)
      (.add text-box)
      (.add pos)
      (.add templates)
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
            r (q/state :r)
            [x y] (get-coord (inc circle) beat)
            [bx by] (get-in @points [key :bound])
            ;; step (s/get-st @sequencer)
            ;; size (/ (dec (get (s/get-p @sequencer key) :size 0)) step)
            ;; [bx by] (get-coord (inc circle) size)
            ]
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
          (q/fill 124 252 0)
          (q/ellipse bx by r r)
          (q/fill 255 0 125)
          (q/ellipse x y r r)

          )
        (when (not display)
          (locking g
            (q/fill 0 0 0)
            (q/ellipse x y (+ r 5) (+ r 5))
            (q/ellipse bx by (+ r 5) (+ r 5))
            (apply q/fill (get-in @points [key :color]))
            (doseq [[o [x y]] (get @points key)]
              (when (and (not (= :color o)) (not (= :bound o)))
                (q/ellipse x y r r))
              ))
          )
        ))
    (.endDraw g))
  )

(defn handle-key []
  (when (and (node-active? @sequencer)
             (-> wheel .getSurface .getNative .getFrame .isDisplayable))
      (ap/with-applet wheel
        ;; (let [g (.getGraphics wheel)]
        ;;   (.beginDraw g)
        ;;   (q/fill 0 0 0)
        ;;   (q/rect 100 50 300 100)
        ;;   (q/fill 255 255 255)
        ;;   (q/text (str "key event: " (q/key-code)) 100 100)
        ;;   (.endDraw g))
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
              cur-beat (+ 1 (* slot step))
              beat (if (= (mod cur-beat (int cur-beat)) 0.0) (int cur-beat) cur-beat)
              beat (+ 1 (* (second new) step))
              beat (if (= (mod beat (int beat)) 0.0) (int beat) beat)
              pattern (nth (keys @points) circle)
              new-pattern (nth (keys @points) (first new))
              display-cursor (q/state :display-cursor)
              text-box (q/state :action-text)
              pos-box (q/state :action-pos)
              key-event @(:key-event (meta wheel))
              cur-action (get-cur-action)
              cur-body (get (s/get-p @sequencer pattern) :body {})]
          ;; (let [g (.getGraphics wheel)
          ;;           [circle slot] (q/state :cursor)]
          ;;       (.beginDraw g)
          ;;       (q/fill 0 0 0)
          ;;       (q/rect 100 50 300 100)
          ;;       (q/fill 255 255 255)
          ;;       (q/text (str key (q/key-code)) 100 100)
          ;;       (.endDraw g))
          (when (or (= :left key) (= :right key) (= :up key) (= :down key) (= 10 (q/key-code)))
            (when (.isControlDown key-event)
              (eval-action "[]"))
            (swap!
             state
             (fn [s]
               (cond
                 (and display-cursor (or (= :left key) (= :right key) (= :up key) (= :down key))
                      (>= (first new) 0) (<= (first new) cnt) (>= (second new) 0) (<= (second new) raw-size))
                 (assoc (assoc s :cursor new) :old-cursor [circle slot])
                 (= 10 (q/key-code)) (assoc s :display-cursor (not display-cursor))
                 true s)))
            (when (.isControlDown key-event)
              (s/set-action @sequencer new-pattern beat cur-action)
              (when (contains? cur-body cur-beat)
               (s/mod-p @sequencer new-pattern :body
                        (assoc cur-body beat (get cur-body cur-beat)))))
            (when (and (or (= :left key) (= :right key)) (.isShiftDown key-event))
              (s/add-p @sequencer
                       (s/p-shift
                        (:data (s/get-p @sequencer pattern))
                        (cond (= :left key) -1
                              (= :right key) 1))
                       pattern)
              (draw-state)
              )
            (when (and (not (nil? text-box)) (not (nil? pos-box)) (.isVisible text-box) (not (= 10 (q/key-code))))
              (.setText text-box (if (contains? (get (s/get-p @sequencer pattern) :body {}) beat)
                                   (get-in (s/get-p @sequencer pattern) [:body beat])
                                   (get-action-str (get-cur-action))))
              (.setText pos-box (str new-pattern " " beat))
              )
            (draw-cursor))
          (when (and (= 120 (q/key-code)) (.isControlDown key-event))
            (eval-action "[]")
            )
          (when (and (= 82 (q/key-code)) (.isControlDown key-event))
            (draw-state)
            )
          (when (and (= 83 (q/key-code)) (.isControlDown key-event))
            (s/mod-p @sequencer new-pattern :size beat)
            (s/stretch-p @sequencer new-pattern beat)
            )
          (when (= :e key)
            (draw-action)
            )
          )
        ))
  )

(defn draw-line [beat]
  (ap/with-applet wheel
    (let [g (.getGraphics wheel)
          display-cursor (q/state :display-cursor)]
      (when (not display-cursor)
        (.beginDraw g)
        (let [r (q/state :r)
              step (s/get-st @sequencer)
              offset (dec (int beat))
              size (get (s/get-sequencer-data @sequencer) :size)
              raw-size (int (/ (- size 1) step))
              prev (if (= 0 offset) raw-size (dec offset))
              cursor (nth (keys @points) (first (q/state :cursor)))]
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
          )
        (.endDraw g))))
  )



(defn draw []
  (let [init (q/state :init)
        d (q/state :d)
        r (q/state :r)
        x (/ (q/width) 2)
        y (/ (q/height) 2)
        step (s/get-st @sequencer)
        coords (gen-coords x y init d @sequencer)
        g (.getGraphics wheel)
        f (q/state :current-fn)]

    (reset! points coords)
    (q/clear)
    (doseq [[k v] coords]
      (apply q/fill (get v :color))
      (doseq [[o [x y]] v]
        (when (and (not (= k :legend)) (number? o))
          (q/ellipse x y r r)
          )
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
                 :action-pos nil
                 :key-event nil
                 :current-fn "draw"
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
