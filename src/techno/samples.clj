(ns techno.samples
  (:use [clojure.java.io]
        [overtone.sc.sample]
        [techno.sequencer :as s]
        )
  (:require [clojure.string :as string])
  )


(defn create-sample-map [path & [nest filter]]
  (let [directory (file path)
        tree (reduce (fn [tree listing]
                       (if (and (.isFile listing)
                                (or (.contains (.getName listing) "wav") (.contains (.getName listing) "aif"))
                                (and (if (fn? filter) (filter listing) true)))
                         (let [nested-path
                               (map (fn [part]
                                      (keyword (string/replace (string/replace part " " "") ":" "")))
                                    (string/split  (subs (.getAbsolutePath listing) (inc (count path)))  #"\\"))
                               disk-path (string/replace (.getAbsolutePath listing) "\\" "\\\\")
                               ]

                           (if (= nest true)
                             (update-in tree nested-path (fn [_] (sample disk-path)))
                             (assoc tree (keyword (string/replace (string/replace (.getName listing) " " "") ":" ""))
                                    (sample disk-path))
                             )
                           )
                         tree
                         )
                       )
                     {}
                     (file-seq directory)
               )
        ]
    tree
    )
  )

(defn group-samples [samples]
  (let [sample-names (sort (map name (keys samples)))]
    (reduce (fn [g sample]
              (let [group (-> (re-seq #"([A-Za-z]+)[^\dA-Za-z]?[\d]+\..*$" sample) first last keyword)
                    inst (keyword sample)]
                (assoc-in g [group inst] (samples inst))
                )) {} sample-names)
    )
  )

(defn build-from-samples [samples pattern & [step args]]
  (let [sounds (reduce into [] samples)
        args (if args args [])
        get-inst (fn [in]
                   (cond (sequential? in) in
                         true (if (string? in)
                                (vector
                                 (second (first
                                          (filter
                                           (fn [[k v]] (.contains (name k) in))
                                           sounds
                                           )))
                                 args))
                         ))
        s (fn [ins]
            (if (or (and (sequential? ins) (= (first ins) :space)) (keyword? ins))
              ins
              (mapcat get-inst ins)))]
    (cond (map? pattern) (zipmap (keys pattern) (map s (vals pattern)))
          (sequential? pattern) (s/build-rest-p (map s pattern) step)
          )
    )
  )

(defn longest [xs ys] (if (> (count xs) (count ys)) xs ys))


(def lcs
  (memoize
   (fn [[x & xs] [y & ys]]
     (cond
      (or (= x nil) (= y nil) ) nil
      (= x y) (cons x (lcs xs ys))
      :else (longest (lcs (cons x xs) ys) (lcs xs (cons y ys)))))))

(def drum-kits (create-sample-map (str (.getCanonicalPath (clojure.java.io/file ".")) "\\musicradar-drum-samples\\Drum Kits") true))
