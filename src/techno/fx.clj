(ns techno.fx
  "Functions to manage pattern effects and signal chains"
  (:use [overtone.core])]
  )

(defonce ^:private pattern-fx (atom {})) ; State containing all effects data
(defonce ^:private pattern-groups (atom {}))
(defonce ^:private bus-pool (atom []))
(defonce ^:private bus-pool-using (atom []))

(defn get-pattern-fx [key]
  (get @pattern-fx key)
  )

(defn add-pattern-fx [pat key nod]
  (swap! pattern-fx assoc-in [pat key] nod)
  )

(defn rm-pattern-fx [pat key]
  (swap! pattern-fx
         (fn [p]
           (if (node? (get-in p [pat key]))
               (kill (get-in p [pat key])))
           (assoc p pat (dissoc (get p pat) key))))
  )

(defsynth p-mixer [audio-bus 10 out-bus 0 volume 1 start-release 0]
      (let [source    (in:ar audio-bus 2)
            source    (* volume source)]
        (detect-silence:ar (select:ar start-release [(dc:ar 1) source]) :action 14)
        (out:ar 0 source)
        ))

(defn handle-pattern-fx [key attrs kill-group & [fx]]
  (when (and (contains? @pattern-groups key) kill-group)
    ;; (kill (get-in @pattern-groups [pattern :id]))
    (doseq [[f x] (get @pattern-fx key)]
      (when (and (node? x) (not (= f :mixer)) (not (= f :group))
                 (node-active? x))
        (kill x)
        ))
    (when (not (nil? (get-in @pattern-fx [key :mixer])))
      (try
        (ctl (get-in @pattern-fx [key :mixer]) :start-release 1)
        (catch Exception e
          (kill (get-in @pattern-groups [key :id]))))
      )
    (when (not (nil? (get-in @pattern-fx [key :bus])))
      (return-bus (get-in @pattern-fx [key :bus]))
      )
    (swap! pattern-groups
           dissoc key)
    (swap! pattern-fx
           dissoc key)
    )
  (when  (and (not kill-group)
              (not (contains? @pattern-groups key))
              (not (contains? attrs :no-group)))
    (let [vol (get attrs :volume 1)
          p-group (group)
          p-bus (get-bus)
          mixer (p-mixer [:tail p-group] p-bus :volume vol)]
      (swap! pattern-groups assoc key p-group)
      (when (not (nil? p-bus))
        (swap! pattern-fx assoc-in [key :mixer] mixer)
        (swap! pattern-fx assoc-in [key :bus] p-bus)
        (swap! pattern-fx assoc-in [key :group] p-group))
      )
    )
  (when (and (map? fx) (contains? @pattern-groups key))
    (let [p-group (get @pattern-groups key)
          p-bus (get-in @pattern-fx [key :bus])]
        (doseq [[k v] fx]
          (swap! pattern-fx assoc-in [key k]
                 (apply (first v)
                        (concat
                         [[:head p-group] :audio-bus p-bus :out-bus p-bus]
                         (rest v))))))
    )
  )

(defn get-bus []
  (let [b (first @bus-pool)]
    (if (not (nil? b))
      (do
        (swap! bus-pool rest)
        (swap! bus-pool-using conj b)
        b)))
  )
(defn return-bus [bus]
  (swap! bus-pool conj bus)
  (swap! bus-pool-using #(filter (fn [i] (not (= i bus))) %))
  )
