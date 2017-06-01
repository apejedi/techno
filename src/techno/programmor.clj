(ns techno.programmor
  (:use [overtone.core]
        [overtone.osc.peer]
        [overtone.osc.util]
        [techno.sketches]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.synths]
        )
  )

(zero-conf-on)

(def server (atom (osc-server 4410 "techno")))
                                        ;(defonce client (atom (osc-client "192.168.1.13" 9000)))

                                        ;(defonce client (atom (osc-client "10.251.243.105" 9000)))
(def client (atom (osc-client "172.31.98.250" 9000)))

(def server-client (atom (osc-client "127.0.0.1" 4410)))

(def state
  (atom
   {
    :cur-pattern nil
    :cur-step nil
    :cur-action nil
    :cur-synth nil
    :scope :pattern
    :patterns {}
    :synths {}
    :synth-params {}
    :lock-params false
    }))

(defmacro functionize [macro]
  `(fn [& args#] (eval (cons '~macro args#))))

(defmacro apply-macro [macro args]
   `(apply (functionize ~macro) ~args))


(defn send-bundle [time msgs]
  (let [msgs (map (fn [[path args]] (apply mk-osc-msg path (osc-type-tag args) args)) msgs)]
    (osc-send-bundle
     @client
     (mk-osc-bundle time msgs))
    )
  )

(defn set-labels [labels color-vals]
  (let [paths (for [i (range 1 5) j (range 1 17)]
                (str "/row" i "/label" j))
        color-paths (for [i (range 4 0 -1) j (range 1 17)]
                      (str "/programPanel/" i "/" j))
        msgs (map
                  (fn [p idx]
                    (let [args (if (> idx (dec (count labels)))
                                 (vector "")
                                 (vector (nth labels idx )))]
                      (apply mk-osc-msg p (osc-type-tag args) args)
                      )
                    ) paths (range 0 (count paths)))
        highlights (map
                (fn [p c]
                  (apply mk-osc-msg p (osc-type-tag (if (not (nil? c)) [1] [0])) (if (not (nil? c)) [1] [0]))
                  ) color-paths color-vals)
        colors (map
                (fn [p c]
                  (apply mk-osc-msg (str p "/color") (osc-type-tag (if (not (nil? c)) [c] ["blue"])) (if (not (nil? c)) [c] ["blue"]))
                  ) color-paths color-vals)]
    (osc-send-bundle
     @client
     (mk-osc-bundle (osc-now) msgs))
    (osc-send-bundle
     @client
     (mk-osc-bundle (+ (osc-now) 1000) highlights))
    (osc-send-bundle
     @client
     (mk-osc-bundle (+ (osc-now) 1000) colors))
    )
  )

(defn update-panel []
  (let [scope (:scope @state)
        labels (cond (= scope :pattern) (map name (keys (get @state :patterns)))
                     (= scope :step) (map str (map (fn [o] (if (= (mod o (int o)) 0.0) (int o) o)) (vec (range 1 17 0.25))))
                     (= scope :action) (map str (range 0 64))
                     (= scope :synth) (map (fn [[k v]] (str k)) (:synths @state))
                     true [])
        colors (vec (map #(cond
                            (= scope :pattern) (cond (= (:cur-pattern @state) (get (vec (keys (get @state :patterns))) %)) "blue"
                                                     (and (node-active? @core/s-player) (not (empty? (s/get-p @core/s-player (get (vec (keys (get @state :patterns))) % 0))))) "green"
                                                     true nil)
                            (= scope :step) (let [p (get-in @state [:patterns (:cur-pattern @state)] {})
                                                  step (s/get-step p)
                                                  size (if (empty? p) 0 (s/p-size p))
                                                  o (inc (* step %))
                                                  o (if (= (mod o (int o)) 0.0) (int o) o)
                                                  action (get p o [])
                                                  has-action (not (nil? (first action)))
                                                  is-terminus (= size (inc (* step %)))]
                                              (cond (= (float (:cur-step @state)) (get (vec (range 1 17 0.25)) %))
                                                    "blue"
                                                (and is-terminus has-action) "purple"
                                                is-terminus "red"
                                                has-action "green"
                                                true nil))
                            (= scope :action) (let [actions (get-in @state [:patterns (:cur-pattern @state) (:cur-step @state)] [])]
                                                (cond (= (:cur-action @state) %) "blue"
                                                  (contains? (vec (partition 2 actions)) %) "green"
                                                  true nil))
                            (= scope :synth)
                            (cond (= (:name (:cur-synth @state)) (get (vec (map (fn [[k v]] (str k)) (:synths @state))) %)) "blue"
                                  true nil)
                            true nil)
                         (range 0 64)))]
    (set-labels labels colors)
    )
  )

(defn add-scope-handlers []
  (osc-handle
   @server "/scope/5/1"
   (fn [msg]
     (let [on (> (first (:args msg)) 0)]
       (when on
         (swap! state assoc :scope :pattern)
         (update-panel)
         ;(set-labels (map str (keys (get @state :patterns))))
         (if (nil? (:cur-pattern @state))
           (swap! state assoc :cur-pattern (first (keys (get @state :patterns)))))
         (osc-send @client "/scope/5/1" 1))
       )))
  (osc-handle
   @server "/scope/4/1"
   (fn [msg]
     (let [on (> (first (:args msg)) 0)]
       (when on
         (let [pattern (get-in @state [:patterns (:cur-pattern @state)] {})
               step (s/get-step pattern)
               coll (range 1 17 0.25)
               ;coll (range 1 (+ (s/p-size pattern) step) step)
               coll (map #(if (= (mod % (int %)) 0.0) (int %) %) coll)
               msgs (map #(let [a (get pattern %1)]
                            (vector
                             (str "/row" (inc (int (/ %2 16))) "/label" (inc (mod %2 16)) "/color")
                             (if (and (sequential? a) (not (nil? (first a))))
                               ["green"] ["yellow"]))
                            ) coll (range 0 (count coll)))]
           (swap! state assoc :scope :step)
           (update-panel)
           ;(set-labels (map str coll))
           ;(send-bundle (+ (osc-now) 1000) msgs)
           (osc-send @client "/scope/4/1" 1)))
       )))
  (osc-handle
   @server "/scope/3/1"
   (fn [msg]
     (let [on (> (first (:args msg)) 0)
           actions (get-in @state [:patterns (:cur-pattern @state) (:cur-step @state)] [])
           msgs (map #(let []
                        (vector
                         (str "/row" (inc (int (/ % 16))) "/label" (inc (mod % 16)) "/color")
                         (if (contains? (vec (partition 2 actions)) %)
                           ["green"] ["yellow"]))
                       ) (range 0 64))]
       (when on
         (swap! state assoc :scope :action)
         (update-panel)
         ;(set-labels (vec (range 0 64)))
         ;(send-bundle (+ (osc-now) 1000) msgs)
         (osc-send @client "/scope/3/1" 1))
       )))
  (osc-handle
   @server "/scope/2/1"
   (fn [msg]
     (let [on (> (first (:args msg)) 0)]
       (when on
         (swap! state assoc :scope :synth)
         (update-panel)
         ;(set-labels (map (fn [[k v]] (str k)) (:synths @state)))
         (osc-send @client "/scope/2/1" 1))
       )))
  )



(defn add-panel-handlers []
  (doseq [r (range 1 5) c (range 1 17)]
    (let [pos (dec (+ (* (- 4 r) 16) c))
          path (str "/programPanel/" r "/" c)]
      (osc-handle
       @server
       path
       (fn [m]
         (let [scope (:scope @state)
               on (> (first (:args m)) 0)
               pattern (get-in @state [:patterns (:cur-pattern @state)] {})
                                        ;step (s/get-step pattern)
               step 0.25
               [coll update key]
               (cond (= scope :pattern)
                     [(vec (keys (:patterns @state))) "/curPattern" :cur-pattern]
                     (= scope :step)
                     [(vec (range 0 64))
                      "/curStep" :cur-step]
                     (= scope :action)
                     [(vec (range 0 64)) "/curAction" :cur-action]
                     (= scope :synth)
                     [(vec (map (fn [[k v]] (str k)) (:synths @state))) "/curSynth" :cur-synth])
               val (cond (= scope :pattern)
                         (nth coll pos)
                         (= scope :step)
                         (if (= (mod (+ 1 (* pos step)) (int (+ 1 (* pos step)))) 0.0) (int (+ 1 (* pos step))) (+ 1 (* pos step)))
                         (= scope :action) pos
                         (= scope :synth)
                         (eval (symbol (str "techno.synths/" (str (nth coll pos))))))
               val-str (cond (= scope :synth) (:name val)
                             true val)
               actions (get-in @state [:patterns (:cur-pattern @state) (:cur-step @state)] [])]
           (when (and on (contains? coll pos))
             (swap! state assoc key val)
             (when (and (:term-mode @state) (= scope :step))
               (swap! state assoc-in [:patterns (:cur-pattern @state)]
                      (into {} (filter (fn [[k v]] (<= k val )) (merge {val []} (get-in @state [:patterns (:cur-pattern @state)])))))
               )
             (when (:clear-mode @state)
               (cond (= scope :step) (swap! state assoc-in [:patterns (:cur-pattern @state) (:cur-step @state)] [])
                     (= scope :action) (swap! state assoc-in [:patterns (:cur-pattern @state) (:cur-step @state)] (vec (concat (subvec actions 0 (* pos 2)) (subvec actions (inc (* pos 2))))))
                     (= scope :pattern) (swap! state assoc-in [:patterns (:cur-pattern @state)] {1.75 []})
                     ))
             (when (and (:apply-mode @state) (or (= scope :action) (= scope :step)))
               (let [action [(:cur-synth @state) (mapcat (fn [[k v]] (vector k (:val v))) (:synth-params @state))]
                     actions (if (and (< (* pos 2) (count actions)) (= scope :action))
                               (assoc (assoc actions (* pos 2) (first action)) (inc (* pos 2)) (second action))
                               (conj actions (first action) (second action)))]
                 (swap! state assoc-in [:patterns (:cur-pattern @state) (:cur-step @state)] actions)))
             (osc-send @client update (str val-str))
             (update-panel)
             (when (and (or (= scope :action) (= scope :step) (= scope :synth)) (not (:lock-params @state)) (not (:apply-mode @state)))
               (let [[synth args] (cond (= scope :synth) [val []]
                                    true (get (vec (partition 2 (get-in @state [:patterns (:cur-pattern @state) (:cur-step @state)] []))) (get @state :cur-action 0)))]
                 (when (not (nil? synth))
                   (let [params (:params synth)
                         param-vals (if (> (count args) 0)
                                      (loop [args args vals {} pos 0 use-key? (keyword? (first args))]
                                        (let [param (if use-key? (first (filter #(= (:name %) (name (first args))) params))
                                                        (nth params pos))
                                              default (if (and (not (nil? param)) (> (:default param) 0)) (:default param) 5)
                                              val (if use-key? (second args) (first args))
                                              scaled-val (scale-range val 0 (* default 5) 0 1)
                                              args (if use-key? (-> args rest rest) (rest args))
                                              vals (if (not (nil? param)) (assoc vals (keyword (:name param)) {:val val :scaled-val scaled-val}) vals)]
                                          (if (> (count args) 0)
                                            (recur args vals (inc pos) (keyword? (first args)))
                                            vals))) {})
                         param-vals (merge (into {} (mapcat #(vector [(keyword (:name %)) {:val (:default %)}]) (:params synth))) param-vals)
                         msgs (mapcat #(vector (vector (str "/synthParam" %) [0]) (vector (str "/synthParamLabel" %) [""]) (vector (str "synthParamVal" %) [""])) (range 1 9))
                         msgs (vec
                               (concat
                                msgs
                                (mapcat (fn [param num]
                                        (let [key (keyword (:name param))
                                              val (get-in param-vals [key :val] (:default param))
                                              scaled (scale-range val 0 (* (if (> (:default param) 0) (:default param) 5) 5) 0 1)
                                              val-str (cond (= (:name param) "note") (str " (" (name (find-note-name (int val))) ")")
                                                            (= (:name param) "freq") (str (format "%.3f" val) " (" (name (find-note-name (hz->midi val))) ")")
                                                            true val)]
                                          (vector
                                            [(str "/synthParam" num) [(float scaled)]]
                                            [(str "/synthParamLabel" num) [(get param :name)]]
                                            [(str "/synthParamVal" num) [val-str]]
                                           )
                                          ))
                                        params (range 1 9))))
                         msgs (conj msgs ["/curSynth" [(:name synth)]])]
                     (send-bundle (osc-now) msgs)
                     (swap! state assoc :cur-synth synth)
                     (swap! state assoc :synth-params param-vals)
                     ))))
             ))
         )))
    )
  )

(defn add-param-handlers []
  (doseq [i (range 1 9)]
    (osc-handle
     @server
     (str "/synthParam" i)
     (fn [m]
       (let [val (first (:args m))
             param (get (into [] (get-in @state [:cur-synth :params] {})) (dec i))
             new-val (scale-range val 0 1 0 (* (get param :default 1) 5))]
         (when (not (nil? param))
           (swap! state assoc-in [:synth-params (keyword (:name param)) :val] new-val)
           (osc-send @client (str "/synthParamVal" i) (format "%.3f" new-val))
           ))))
    )
  )
(defn add-button-handlers []
  (osc-handle
   @server
   "/play"
   (fn [m]
     (when (> (first (:args m)) 0)
       (apply (:cur-synth @state) (mapcat (fn [[k v]] (vector k (:val v))) (:synth-params @state))))
     ))
  (osc-handle
   @server
   "/lock"
   (fn [m]
     (when (> (first (:args m)) 0)
       (swap! state assoc :lock-params true)
       (osc-send @client "/lock" 1))
     (when (< (first (:args m)) 1)
       (swap! state assoc :lock-params false)
       (osc-send @client "/lock" 0))
     ))
  (osc-handle
   @server
   "/apply"
   (fn [m]
     (when (> (first (:args m)) 0)
       (swap! state assoc :apply-mode true)
       (osc-send @client "/apply" 1))
     (when (< (first (:args m)) 1)
       (swap! state assoc :apply-mode false)
       (osc-send @client "/apply" 0))
     ))
  (osc-handle
   @server
   "/add"
   (fn [m]
     (when (> (first (:args m)) 0)
       (let [actions (get-in @state [:patterns (:cur-pattern @state) (:cur-step @state)] [])
             actions (vec (concat actions [(:cur-synth @state) (vec (mapcat (fn [[k v]] (vector k (:val v))) (:synth-params @state)))]))]
         (swap! state assoc-in [:patterns (:cur-pattern @state) (:cur-step @state)] actions))
       (when (= (:scope @state) :action)
         (osc-send @server-client "/scope/3/1" 1))
       (when (= (:scope @state) :step)
         (osc-send @server-client "/scope/4/1" 1))
       (osc-send @client "/add" 1))
     (when (< (first (:args m)) 1)
       (osc-send @client "/add" 0))
     ))
  (osc-handle
   @server
   "/clear"
   (fn [m]
     (when (> (first (:args m)) 0)
       (swap! state assoc :clear-mode true)
       (osc-send @client "/clear" 1))
     (when (< (first (:args m)) 1)
       (swap! state assoc :clear-mode false)
       (osc-send @client "/clear" 0)
       (when (= (:scope @state) :action)
         (osc-send @server-client "/scope/3/1" 1))
       (when (= (:scope @state) :step)
         (osc-send @server-client "/scope/4/1" 1))
       )))
  (osc-handle
   @server
   "/update"
   (fn [m]
     (when (> (first (:args m)) 0)
       (s/add-p @core/s-player (get-in @state [:patterns (:cur-pattern @state)]) (:cur-pattern @state))
       (osc-send @client "/update" 1))
     (when (< (first (:args m)) 1)
       (osc-send @client "/update" 0))
     ))
  (osc-handle
   @server
   "/term"
   (fn [m]
     (when (> (first (:args m)) 0)
       (swap! state assoc :term-mode true)
       (osc-send @client "/term" 1))
     (when (< (first (:args m)) 1)
       (swap! state assoc :term-mode false)
       (osc-send @client "/term" 0))
     ))
  )


(defn add-pattern [key data]
  (swap! state assoc-in [:patterns key] data)
  nil
  )


(comment
  (do
    (swap! state assoc :patterns {:drum1 {1.75 []} :drum2 {1.75 []} :drum3 {1.75 []} :drum4 {1.75 []}
                                  :motif1 {1.75 []} :motif2 {1.75 []} :motif3 {1.75 []} :motif4 {1.75 []}
                                  :harmony1 {1.75 []} :harmony2 {1.75 []} :harmony3 {1.75 []} :harmony4 {1.75 []}})
    (swap! state assoc :synths (into {} (filter #(.contains (str (type (val %))) "overtone.sc.synth") (ns-publics 'techno.synths))))
    nil)
  (doseq [i (range 1 9)]
      (in-osc-bundle @client (osc-now)
                     (osc-send @client (str "/synthParam" i) 0)
                     (osc-send @client (str "/synthParamVal" i) "asc")
                     (osc-send @client (str "/synthParamLabel" i) "sac")))

  (do
    (core/start-player)
    (on-latest-trigger
     @core/s-player (get (s/get-sequencer-data @core/s-player) :uid)
     (fn [b]
       (osc-send @client (str "/led" (dec (int b))) 0)
       (osc-send @client (str "/led" (int b)) 1)
                                        ;(println b)
       )
     :test)
    )
  (s/set-sp @core/s-player 2)

  (kill @core/s-player)
  (add-panel-handlers)
  (add-scope-handlers)
  (add-button-handlers)


  (osc-send-bundle
   @client
   (mk-osc-bundle (osc-now)
    (vector
     (apply mk-osc-msg "/row1/label1" (osc-type-tag ["asc"]) ["sasvas"])
     (apply mk-osc-msg "/row1/label2" (osc-type-tag ["asc"]) ["ascasc"])))
   )

  (osc-send @client "/row1/label1/color"  "yellow")
  (osc-send @client "/programPanel/3/5" 1)
  (set! server (osc-server 4410 "techno"))
  (osc-listen
   @server
   (fn [m]
     (println m)
     )
   :debug)

  (osc-type-tag [(int 1)])
  (osc-rm-listener @server :debug)
  (osc-send @client "/programPanel/4/3" 0)
  (osc-send-bundle
   @client
   (mk-osc-bundle (osc-now)
    (vector
     (apply mk-osc-msg "/programPanel/4/3/color" (osc-type-tag ["asc"]) ["yellow"])
     (apply mk-osc-msg "/programPanel/4/3" (osc-type-tag [1]) [1])))
   )
  (osc-send-bundle
   @server-client
   (mk-osc-bundle
    (osc-now)
    [{:path "/row1/label1" :type-tag "s", :args ["test"]}]
    ;; [{:path "/programPanel/4/3/color" :type-tag "s" :args '("green")}
    ;;  {:path "/programPanel/4/3" :type-tag "i" :args '((int 1))}]
    )
   )
(osc-send @client "/programPanel/4/1" 0)
  (osc-send @server-client
            "/test"
                 "test")
  )
