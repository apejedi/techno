(ns techno.ring2
  (:use [techno.samples :only [drum-kits]]
        [overtone.sc.trig]
        [overtone.sc.node]
        [overtone.music.pitch]
        [overtone.libs.event :only [on-event event]])
  (:require [techno.player :as p]
            [quil.core :as q]
            [quil.applet :as ap]
            )
  (:import [java.awt Color]
           [javax.swing JComboBox JTextArea GroupLayout JFrame JTextField JButton]
           (java.awt Dimension Font Color)
           (java.awt.event ActionListener ActionEvent)))

(defonce player (atom nil))
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
          size (p/get-state @player :size)
          theta (/ 6.28319 size)
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
  (let [s-size (p/get-state player :size)
        s-div (p/get-state player :div)
        theta (/ 6.28319 s-size)
        h 0.84
        ratio 0.618033988749895
        markers (range 1 (inc s-size))
        patterns (conj (into [] (p/get-p player))
                       [:legend (p/stretch-p {:div s-div 1 {1 [:d []]}} s-size)])
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
                     (let [v (if (map? v) v {:div 4 1 {2 []}})
                           size (p/p-size v s-div)]
                         (doseq [o (range 1 (inc s-size))]
                           (let [pos (p/get-pos o (:div v) size s-div)
                                 a (get-in v pos)
                                 pts (get coord-map k {})]
                             (when (and (> (first pos) 0)
                                        (sequential? a)
                                        (not (nil? (first a))))
                               (assoc! coord-map
                                       k (assoc pts o (gen-coord n (dec o))))
                               )))
                         (assoc! coord-map k
                                 (assoc (get coord-map k {}) :bound
                                        (gen-coord n (dec size))))
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
          pattern (p/get-p @player (nth (keys @points) circle))
          size (p/p-size pattern (p/get-state @player :div))
          action (get-in pattern (p/get-pos beat (:div pattern)
                                            size (p/get-state @player :div)))]
      action
      ))
  )

;; (defn get-action-str [action ]
;;   (s/get-action-str action drum-kits "drum-kits"))


(defn eval-action [action]
  (ap/with-applet wheel
    (let [[circle beat] (q/state :cursor)
          pattern (nth (keys @points) circle)
          data (p/get-p @player pattern)
          s-div (p/get-state @player :div)
          size (p/p-size data s-div)
          pos (p/get-pos beat (:div data) size s-div)
          x (if (> beat size) (p/add-p @player (p/stretch-p data pos) pattern))
          action (clojure.string/replace action "\n" "")
          res
          (load-string (str " (import java.util.concurrent.ThreadLocalRandom) (use
'[clojure.core]
'[overtone.core]
        '[overtone.inst.synth]
        '[techno.core :as core]
        '[techno.synths]
        '[techno.drum-patterns]
        '[techno.drums]
        '[techno.samples])
    (require '[techno.player :as p])
(p/mod-p techno.core/player
" pattern " " (first pos) " " (second pos) " " action ")"))]
      ;(println res)
      (p/mod-p @player pattern :body (first pos) (second pos) action)
      ))
  (draw-state)
  )


;; (defn draw-action []
;;   (let [frame (JFrame. "Beat Action")
;;         text-box (JTextArea.)
;;         kits (sort (map name (keys drum-kits)))
;;         kit (JComboBox. (into-array String kits))
;;         ;; sounds (JComboBox. (into-array String (sort (map name (keys (get drum-kits (keyword (first kits))))))))
;;         sounds (JComboBox. (into-array
;;                             SampleItem
;;                             (map
;;                              #(SampleItem. {:display % :value (vector (keyword (first kits)) (keyword %))})
;;                              (sort (map name (keys (get drum-kits (keyword (first kits)))))))))
;;         add (JButton. "Add")
;;         eval (JButton. "Eval")
;;         play (JButton. "Play")
;;         filter-box (JTextField.)
;;         pos (JTextField.)
;;         templates (JComboBox. (into-array String ["chord" "rand"]))
;;         key-handler (fn [^String selected]
;;                       (.removeAllItems sounds)
;;                       (doseq [s (keys (get drum-kits (keyword selected)))]
;;                         (.addItem sounds
;;                                   (SampleItem. {:display (name s) :value (vector (keyword selected) s)})
;;                                   ;(name s)
;;                                   )))
;;         show-action (fn []
;;                       (.setText text-box (s/get-action-str (get-cur-action))))
;;         add-action (fn []
;;                      (ap/with-applet wheel
;;                        (let [[circle beat] (q/state :cursor)
;;                              pattern (nth (keys @points) circle)
;;                              step (s/get-st @sequencer)
;;                              beat (+ 1 (* beat step))
;;                              k (keyword (.getSelectedItem kit))
;;                              snd (.getSelectedItem sounds)
;;                              ;snd (keyword (.getSelectedItem sounds))
;;                              action (get-cur-action)
;;                              action-text (s/get-action-str (vec (conj action (get-in drum-kits (.getValue snd)) [])))
;;                              ;action-text (get-action-str (vec (conj action (get-in drum-kits (vector k snd)) [])))
;;                              ]
;;                          (.setText text-box action-text))))]
;;     (.setLayout frame nil)
;;     (ap/with-applet wheel
;;       (swap! (q/state-atom)
;;              (fn [s] (assoc (assoc s :action-text text-box) :action-pos pos))))
;;     (doto text-box
;;       (.setBackground Color/BLACK)
;;       (.setForeground Color/GREEN)
;;       (.setEditable true)
;;       (.setLineWrap true)
;;       (.setWrapStyleWord true)
;;       (.setFont (Font. "Monospaced" Font/PLAIN 14))
;;       (.setCaretColor Color/white))
;;     (doto pos
;;       (.setBackground Color/BLACK)
;;       (.setForeground Color/GREEN)
;;       (.setFont (Font. "Monospaced" Font/PLAIN 14)))

;;     (.setBounds kit 10 10 150 20)
;;     (.setBounds sounds 160 10 200 20)
;;     (.setBounds filter-box 10 35 150 20)
;;     (.setBounds play 160 35 100 20)
;;     (.setBounds add 380 10 60 20)
;;     (.setBounds eval 450 10 60 20)
;;     (.setBounds pos 520 10 160 20)
;;     (.setBounds templates 10 70 150 20)
;;     (.setBounds text-box 10 130 600 200)
;;     (.addActionListener kit
;;                         (reify ActionListener
;;                           (actionPerformed [this e]
;;                             (key-handler (.getSelectedItem (.getSource e)))
;;                             )))
;;     (.addActionListener sounds
;;                         (reify ActionListener
;;                           (actionPerformed [this e]
;;                             (let [snd (.getSelectedItem sounds)
;;                                   ]
;;                               (when (not (nil? snd))
;;                                   ((get-in drum-kits (.getValue snd))))
;;                               )
;;                             (show-action)
;;                             )))
;;     (.addActionListener add
;;                         (reify ActionListener
;;                           (actionPerformed [this e]
;;                             (add-action)
;;                             )))
;;     (.addActionListener filter-box
;;                         (reify ActionListener
;;                           (actionPerformed [this e]
;;                             (let [texts (clojure.string/split (.getText filter-box) #" ")]
;;                               (.removeAllItems sounds)
;;                               (doseq [[kit snds] drum-kits]
;;                                 (doseq [snd (keys snds)]
;;                                   (when
;;                                       (every? #(or (.contains
;;                                                     (.toLowerCase (name kit))
;;                                                     (.toLowerCase %))
;;                                                    (.contains (.toLowerCase (name snd))
;;                                                               (.toLowerCase %)))
;;                                              texts)
;;                                       (.addItem sounds
;;                                                 (SampleItem.
;;                                                  {:display (name snd)
;;                                                   :value (vector kit snd)})
;;                                                 ))))
;;                               )
;;                             )))
;;     (.addActionListener play
;;                         (reify ActionListener
;;                           (actionPerformed [this e]
;;                             (let [snd (.getSelectedItem sounds)]
;;                               ((get-in drum-kits (.getValue snd)))
;;                               )
;;                             )))
;;     (.addActionListener eval
;;                         (reify ActionListener
;;                           (actionPerformed [this e]
;;                             (eval-action (.getText text-box))
;;                             )))
;;     (.addActionListener templates
;;                         (reify ActionListener
;;                           (actionPerformed [this e]
;;                             (let [selected (.getSelectedItem (.getSource e))]
;;                               (.setText
;;                                text-box
;;                                (cond (= selected "chord") "(s/chord-p\ninst\n (chord) [])"
;;                                      (= selected "rand") "(let [coll (atom (s/x-seq []))]\n
;; [(fn [] (reset! coll (rest @coll)) \n
;; (doseq [n (chord-degree (first @coll) :B3 :minor)] \n
;; (overpad :note n))\n
;; ) []])"))
;;                               )
;;                             )))
;;     (doto frame
;;       (.add kit)
;;       (.add sounds)
;;       (.add add)
;;       (.add eval)
;;       (.add play)
;;       (.add filter-box)
;;       (.add text-box)
;;       (.add pos)
;;       (.add templates)
;;       (.setSize 700 300)
;; ;      (.pack)
;;       (.setVisible true)
;;       )
;;     )
;;   )

(defn draw-cursor []
  (let [g (.getGraphics wheel)]
    ;; (let []
    ;;       (.beginDraw g)
    ;;       (q/fill 0 0 0)
    ;;       (q/rect 100 150 300 100)
    ;;       (q/fill 255 255 255)
    ;;       (q/text (str (q/state :cursor) " " (q/state :old-cursor)) 100 200)
    ;;       (.endDraw g))
    (.beginDraw g)
    (ap/with-applet wheel
      (let [display (q/state :display-cursor)
            [circle beat] (q/state :cursor)
            [old-circle old-beat] (q/state :old-cursor)
            key (nth (keys @points) circle)
            old (nth (keys @points) old-circle)
            r (q/state :r)
            [x y] (get-coord (inc circle) (dec beat))
            [bx by] (get-in @points [key :bound])]
        (when display
          (q/fill 0 0 0)
          (apply q/ellipse (conj (apply get-coord (vector (inc old-circle) (dec old-beat))) r r))
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
  (when (and (p/active? @player)
             (-> wheel .getSurface .getNative .getFrame .isDisplayable))
      (ap/with-applet wheel
        (let [[circle slot] (q/state :cursor)
              cnt (count (keys (p/get-p @player)))
              size (p/get-state @player :size)
              state (q/state-atom)
              key (q/key-as-keyword)
              n-circle (+ circle (cond (= key :up) 1
                                       (= key :down) -1
                                       true 0))
              pattern (nth (keys @points) circle)
              new-pattern (nth (keys @points) n-circle)
              data (p/get-p @player new-pattern)
              div (p/get-p @player new-pattern :div)
              n-div (/ (p/get-state @player :div) div)
              step (fn [d o]
                     (cond (= d :left) (dec (int (* (Math/floor (/ o n-div)) n-div)))
                           (= d :right) (inc (int (* (Math/ceil (/ o n-div)) n-div)))
                           true o))
              n-slot (mod (step key slot) size)
              n-slot (if (= 0 n-slot) size n-slot)
              display-cursor (q/state :display-cursor)
              text-box (q/state :action-text)
              pos-box (q/state :action-pos)
              key-event @(:key-event (meta wheel))
              cur-action (get-cur-action)
              cur-body (get (p/get-p @player pattern) :body {})
              old-pos (p/get-pos slot div size (p/get-state @player :div))
              pos (p/get-pos n-slot div size (p/get-state @player :div))]
          (let [g (.getGraphics wheel)
                    [circle slot] (q/state :cursor)]
                (.beginDraw g)
                (q/fill 0 0 0)
                (q/rect 100 50 300 100)
                (q/fill 255 255 255)
                (q/text (str pos) 100 100)
                (.endDraw g))
          (when (or (= :left key) (= :right key) (= :up key) (= :down key) (= 10 (q/key-code)))
            ;; (when (.isControlDown key-event)
            ;;   (eval-action "[]"))
            (swap!
             state
             (fn [s]
               (cond
                 (and display-cursor
                      (or (= :left key) (= :right key) (= :up key) (= :down key)))
                 (assoc (assoc s :cursor [n-circle n-slot]) :old-cursor [circle slot])
                 (= 10 (q/key-code)) (assoc s :display-cursor (not display-cursor))
                 true s)))
            ;; (when (.isControlDown key-event)
            ;;   (p/set-action @player new-pattern beat cur-action)
            ;;   (when (contains? cur-body cur-beat)
            ;;     (p/mod-p @player new-pattern :body
            ;;              (assoc cur-body beat (get cur-body cur-beat)))))
            (when (and (or (= :left key) (= :right key)) (.isShiftDown key-event))
              (p/mod-p @player new-pattern (first pos) (second pos)
                       (p/get-p @player pattern (first old-pos) (second old-pos)))
              (p/mod-p @player pattern (first old-pos) (second old-pos) nil)
              (draw-state)
              )
            ;; (when (and (not (nil? text-box)) (not (nil? pos-box)) (.isVisible text-box) (not (= 10 (q/key-code))))
            ;;   (.setText text-box (if (contains? (get (p/get-p @player pattern) :body {}) beat)
            ;;                        (get-in (p/get-p @player pattern) [:body beat])
            ;;                        (p/get-action-str (get-cur-action))))
            ;;   (.setText pos-box (str new-pattern " " beat))
            ;;   )
            (draw-cursor)
            )
          (when (and (= 73 (q/key-code)) (.isControlDown key-event)) ;i
            (p/add-p @player
                     (p/stretch-p
                      data
                      [(inc
                        (apply max (filter number? (keys data))))
                       div])
                     new-pattern)
            (draw-state)
            (draw-cursor)
            )
          (when (and (= 73 (q/key-code)) (.isShiftDown key-event)) ;i
            (p/add-p @player
                     (assoc-in
                      data
                      [(inc
                        (apply max (filter number? (keys data))))
                       div] [])
                     new-pattern)
            (draw-state)
            (draw-cursor)
            )
          (when (and (= 68 (q/key-code)) (.isShiftDown key-event)) ;d
            (p/add-p @player
                     (assoc-in
                      (p/stretch-p
                       data
                       [(-
                         (apply max (filter number? (keys data))) 2)
                        div])
                      [(dec (apply max (filter number? (keys data))))
                       div]
                      [])
                     new-pattern)
            (draw-state)
            (draw-cursor)
            )
          (when (and (= 68 (q/key-code)) (.isControlDown key-event)) ;d
            (p/add-p @player
                     (p/stretch-p
                      data
                      [(dec
                        (apply max (filter number? (keys data))))
                       div])
                     new-pattern)
            (draw-state)
            (draw-cursor)
            )
          (when (and (= 88 (q/key-code)) (.isControlDown key-event)) ;x
            (eval-action "[]")
            )
          (when (and (= 67 (q/key-code)) (.isControlDown key-event)) ;c
            (swap! state assoc :copy (p/get-p @player new-pattern (first pos) (second pos)))
            )
          (when (and (= 86 (q/key-code)) (.isControlDown key-event)) ;v
            (p/mod-p @player new-pattern (first pos) (second pos) (q/state :copy))
            )
          (when (and (= 82 (q/key-code)) (.isControlDown key-event)) ;r
            (draw-state)
            )
          (when (and (= 83 (q/key-code)) (.isControlDown key-event)) ;s
            (p/add-p
             @player
             (p/stretch-p (p/get-p @player new-pattern)
                          pos)
             new-pattern)
            )
          ;; (when (= :e key)
          ;;   (draw-action)
          ;;   )
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
              size (p/get-state @player :size)
              prev (if (= 0 beat) size (dec beat))]
          (doseq [[k v] @points]
            (when (not (= :legend k))
              (when (contains? v prev)
                (q/fill 0 0 0)
                (apply q/ellipse (conj (vec (get v prev)) (+ r 3) (+ r 3)))
                (apply q/fill (get v :color))
                (apply q/ellipse (conj (vec (get v prev)) r r)))
              (apply q/fill [255 255 0])
              (when (contains? v beat)
                (apply q/ellipse (conj (vec (get v beat)) r r)))
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
        coords (gen-coords x y init d @player)
        g (.getGraphics wheel)
        f (q/state :current-fn)
        div (p/get-state @player :div)]

    (reset! points coords)
    (q/clear)
    (doseq [[k v] coords]
      (apply q/fill (get v :color))
      (doseq [[o [x y]] v]
        (when (and (not (= k :legend)) (number? o))
          (q/ellipse x y r r)
          )
        (when (and (= :legend k) (number? o))
          (let [m (mod (dec o) div)
                o (cond (= m 0) (inc (/ (dec o) div)) (= o 1) 1 true -1)]
            (when (> o 0)
                (q/text (str o) x y))))
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


(defn ring [pl & [init r d]]
  (let [r (if r r 2)
        d (if d d 4)]
    (reset! player pl)
    (p/add-listener pl :ring draw-line)
    (q/defsketch wheel
      :setup setup
      :draw draw
      :features [:present]
      :key-pressed handle-key
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
                 :cursor [0 1]
                 :display-cursor false
                 :old-cursor [0 1]
                 :action-text nil
                 :action-pos nil
                 :key-event nil
                 :current-fn "draw"
                 ;:action-box (g4p_controls.GTextArea. ^processing.core.PApplet wheel 50 100 290 300)
                 })
         ))
      )
    wheel
    )
  )
