(ns techno.samples
  (:use [clojure.java.io]
        [overtone.sc.sample]
        )
  (:require [clojure.string :as string])
  )


(defn create-sample-map [path & [nest filter]]
  (let [directory (file path)
        tree (reduce (fn [tree listing]
                       (if (and (.isFile listing)
                                (or (.contains (.getName listing) "wav") (.contains (.getName listing) "aiff"))
                                (and (if (fn? filter) (filter listing) true)))
                         (let [nested-path
                               (map (fn [part]
                                      (keyword (string/replace (string/replace part " " "") ":" "")))
                                    (string/split  (subs (.getAbsolutePath listing) (inc (count path)))  #"\\"))
                               disk-path (string/replace (.getAbsolutePath listing) "\\" "\\\\")
                               ]

                           (if (= nest true)
                             (update-in tree nested-path (fn [_] (sample disk-path)))
                             (assoc tree (keyword (.getName listing)) (sample disk-path))
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


;; (def sample-test (create-sample-map "D:\\musicradar-drum-samples\\musicradar-drum-samples\\Assorted Hits\\Kicks\\Kes Kick"))

;; (doseq [[_ s] sample-test]
;;   (s)
;;   (java.lang.Thread/sleep 1000)
;;   )
