(ns techno.core
  (:use [overtone.core]
        [techno.ring :as r]
        )
  (:require [techno.sequencer :as s]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.string :as string]
            [overtone.midi :as midi]
            [techno.core :as core]))
(defonce s-player (atom nil))
(declare player)


(comment
  (if (or (nil? player) (not (node-active? player)))
      (def player (s/get-s
                   (/ 80 60)
                   )))

  (on-event [:midi :note-on]
            (fn [m]
              (println m)
              (let [note (:note m)]
                ;; (prophet :freq (midi->hz note)
                ;;          :decay 5
                ;;          :rq 0.6
                ;;          :cutoff-freq 1000)
                ))
            ::prophet-midi)

  (s/set-size player 2.875)
  (s/set-sp player 0.1)
;  (s/mod-p player :pattern10 :use-counter true)
  (s/set-sp player (/ 120 60))
  (s/set-st player (double (/ 1 8)
                           ))
  (s/set-st player 0.25)
  (r/ring player 100 10 50)

  (sweet :dur 0.2)
  (eval-action
   "[sweet []]")

  (s/rm-p player :kick)
  (r/draw-state)
  (r/draw-line 5)
  (r/gen-coords 500 500 5 4 player)
  (s/dec-amp player :shkr)
  (let [v 2]
    (ctl 14 :volume v)
    (ctl 15 :volume v))
  (remove-event-handler ::server-audio-clipping-warner-vol)
  )

(defn get-patterns []
  (if (and (not (nil? player)) (node-active? player))
    (keys (s/get-p player))
    )
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

(defn get-pattern-str [pattern & [type sequencer]]
  (let [type (if type type "rest")]
    (if (= type "map")
      (apply
       str
       (concat
        ["{
"]
        (map
         (fn [[k v]]
           (str k " "
                (s/get-action-str v techno.samples/drum-kits "drum-kits")
                "
")) (get (s/get-p player pattern) :data))
        ["}"]))
      (str
              "(s/build-map-p
" (vec (map #(if (sequential? %)
               (s/get-action-str % techno.samples/drum-kits "drum-kits")
               %)
            (s/build-rest-p (get (s/get-p player pattern) :data)))) "
)"))
    )
  )

(defn- find-match [raw text]
  (let [raw-r (clojure.java.io/reader (.getBytes (string/replace raw " " "")))
        text-r (clojure.java.io/reader (.getBytes text))
        cur (StringBuilder.)]
    (.mark raw-r 100000)
    (loop [r (.read raw-r) t (.read text-r) cur cur]
      ;; (println (str "cur: "(.toString cur)) (str "r: " (if (> r -1) (char r) r)) (str "t: " (if (> t -1) (char t) t)))
      (if (or (= -1 t) (= -1 r))
        (if (> (.length cur) 0)
          (.toString cur)
          raw)
        (if (= r t)
          (recur (.read raw-r) (.read text-r) (.append cur (char t)))
          (if (not (= -1 (.indexOf [9 32 10 59] t)))
            (let [c (loop [c t]
                      (if (and (= t 59) (not (= c 10)))
                        (do (.append cur (char c)) (recur (.read text-r)))
                        (if (and (not (= -1 (.indexOf [9 32 10] t))) (not (= -1 (.indexOf [9 32 10] c))))
                          (do (.append cur (char c)) (recur (.read text-r)))
                          c)))]
              (recur r c cur))
            (recur
             (do (.reset raw-r) (.read raw-r))
             (.read text-r)
             (do (.setLength cur 0) cur)))
          ))
        )
    )
  )

(defn get-patterns-from-string [data & [sketch return-map?]]
  (let [text data
        data (string/replace
              (string/replace data " #(" " \\#(")
              " @" " \\@")
        ;r (readers/source-logging-push-back-reader data)
        r (readers/indexing-push-back-reader data)
        get-val (fn [raw start end]
                  (find-match
                   (string/replace
                    (string/replace
                     (string/replace raw #"\\# ([^\s]+)" "#$1")
                     #"\\@ ([^\s]+)" "@$1")
                    #"," "")
                   (string/join "\n"
                    (subvec
                     (string/split-lines text)
                     (dec start)
                     (dec end)))))
        start (readers/get-line-number r)]
    (loop [cur (edn/read {:eof false} r) patterns {}
           start start end (readers/get-line-number r)]
      (if cur
        (let [[e f g h] cur
              [a b c d] (map str [e f g h])
              patterns (cond (and (.contains a "add-p") (.startsWith c "("))
                             (assoc patterns d (get-val c start end))
                             (and (= "def" a) (= \{ (first c))
                                  (or (not sketch) (nil? sketch) (= b sketch)))
                             (reduce
                              (fn [m [k v]]
                                (assoc m (str k) (get-val (str v) start end)))
                              patterns
                              g)
                             (= "swap!" a)
                             (assoc patterns (str (keyword b)) (get-val c start end))
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
