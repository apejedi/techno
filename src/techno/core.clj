(ns techno.core
  (:use [overtone.core]
        [techno.sequencer :as s]
        )

  (:require [techno.core :as core]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.string :as string]))
(defonce s-player (atom nil))
(declare player)
(comment
  (if (or (nil? player) (not (node-active? player)))
      (def player (s/get-s
                   (/ 80 60)
                   )))
  (s/set-size player 3.75)
  (s/set-sp player 0.1)
  (s/set-sp player (/ 120 60))
  (s/set-st player (double (/ 1 4)
                           ))
  (kill player)
  )

(defn get-patterns []
   (s/get-p player)
  )
(defn start-player []
  (let [player @s-player]
      (if (or (nil? player) (not (node-active? player)))
        (swap!
         s-player
         (fn [_]
           (s/get-s
            (/ 80 60))))))
  )

(defn get-patterns-from-string [data & [sketch return-map?]]
  (let [data (string/replace
              (string/replace data " #(" " \\#(")
              " @" " \\@")
        ;r (readers/source-logging-push-back-reader data)
        r (readers/indexing-push-back-reader data)
        get-val (fn [body]
                  (string/replace
                   (string/replace body #"\\# ([^\s]+)" "#$1")
                   #"\\@ ([^\s]+)" "@$1"))
        start (readers/get-line-number r)]
    (loop [cur (edn/read {:eof false} r) patterns {}
           start start end (readers/get-line-number r)]
      (if cur
        (let [[e f g h] cur
              [a b c d] (map str [e f g h])
              patterns (cond (and (.contains a "add-p") (.startsWith c "("))
                             (assoc patterns d (get-val c))
                             (and (= "def" a) (= \{ (first c))
                                  (or (not sketch) (nil? sketch) (= b sketch)))
                             (reduce
                              (fn [m [k v]]
                                (assoc m (str k) (get-val (str v))))
                              patterns
                              g)
                             (= "swap!" a)
                             (assoc patterns (str (keyword b)) (get-val c))
                             (= "comment" a)
                             (merge patterns (get-patterns-from-string (apply str (rest cur)) nil true))
                             (= "do" a)
                             (merge patterns (get-patterns-from-string (apply str (rest cur)) nil true))
                             true patterns)]
          (recur (edn/read {:eof false} r) patterns end (readers/get-line-number r)))
        (if return-map?
          patterns
          (seq (map seq patterns))))
      )
    )
  )
