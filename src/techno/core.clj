(ns techno.core
  (:import (java.io InputStream FileInputStream))
  (:use [overtone.core]
        [techno.synths]
        [techno.samples]
        )
  (:require [techno.sequencer :as s]
            [techno.player :as p]
            [techno.ring :as r]
            [techno.ring2 :as r2]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.string :as string]
            [overtone.midi :as midi]
            [techno.core :as core]))
(defonce s-player (atom nil))
(declare player)
;; (declare synth-grp)

(comment
  (if (or (nil? player) (not (node-active? player)))
    (let [bus (control-bus)
          active (atom false)]
      (on-event
       [:midi nil]
       (fn [m]
           (when (.contains (name (:status m)) "start")
             (reset! active true))
           (when (and (= (:status m) :timing-clock))
             (control-bus-set! bus 1)
             )
           )
       :midi-clock)
      (def player (s/midi-s bus)))
    )

  (if (or (nil? player) (not (node-active? player)))
      (def player (s/get-s
                   (/ 80 60)
                   )))
  (s/reset-s core/player)

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
  (r2/ring player 100 10 50)
  (s/reset-s player)
  (techno.grid/mk-grid
   6 6 core/player
   (into []
         (mapcat
          #(vector [(-> %1 str keyword) [%2 []]])
          (concat (range 0 10)
                  (map name [:q :w :e :r :t :y :u :i :o :p :a :s :d :f :g :h :j :k :l :z :x :c :v :b :n :m]))
          (concat
           [o-kick  b-kick r-kick b-snr dirty-kick g-kick o-snr o-hat]
           (flatten (map vals (vals (group-samples (drum-kits :claves)))))
           (flatten (map vals (vals (group-samples (drum-kits :congas)))))
           ))))

  (sweet :dur 0.2)
  (eval-action
   "[sweet []]")

  (s/rm-p player :kick)
  (r/draw-state)
  (r/draw-line 5)
  (r/gen-coords 500 500 5 4 player)
  (s/dec-amp player :shkr)
  (let [v 1]
    (ctl 14 :volume v)
    (ctl 15 :volume v))
  (remove-event-handler ::server-audio-clipping-warner-vol)
  (remove-event-handler :midi-clock)
  )

(defn get-patterns []
  (cond (p/active? player) (keys (p/get-p player))
        (and (node? player) (not (nil? player)) (node-active? player))
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

(defn player-active? []
  (p/active? player)
  )

(defn get-pattern-str [pattern & [type sequencer]]
  (let [type (if type type "rest")
        pattern (if (keyword? pattern) (if (node? player)
                                         (get (s/get-p player pattern) :data)
                                         (p/get-p player pattern)) pattern)
        get-p (if (contains? pattern :div) p/get-p s/get-p)
        build-rest-p (if (contains? pattern :div) p/build-rest-p s/build-rest-p)
        is-phrase? (if (contains? pattern :div) p/is-phrase? s/is-phrase?)
        build-phrase-p (if (contains? pattern :div) p/build-phrase-p s/build-phrase-p)
        alias (if (contains? pattern :div) "p" "s")]
    (if (= type "map")
      (apply
       str
       (concat
        ["{
"]
        (map
         (fn [[k v]]
           (str k " "
                (techno.sequencer/get-action-str v techno.samples/drum-kits "drum-kits")
                "
")) (get (get-p player pattern) :data))
        ["}"]))
      (let [is-phrase (is-phrase? pattern)
            div (if (contains? pattern :div) (/ 1 (:div pattern)) (s/get-step pattern))
            p (build-rest-p pattern)]
        (if (not is-phrase)
          (let [strs (map #(if (sequential? %)
                         (techno.sequencer/get-action-str % techno.samples/drum-kits "drum-kits")
                         (str % "\n"))
                      p)
            p-str (clojure.string/join " " strs)]
              (str "(" alias "/build-map-p
[" p-str "
] " div ")"))
          (let [[inst phrase-p step params] (build-phrase-p pattern)]
            (str "(" alias "/phrase-p
 " (:name inst) "
" phrase-p "
" step " 0 " params ")")
            )
          )
        )
      )
    )
  )

(defn- find-match [raw text]
  (let [raw-r (clojure.java.io/reader (.getBytes (string/replace raw " " "")))
        text-r (clojure.java.io/reader (.getBytes text))
        cur (StringBuilder.)]
    (.mark raw-r 100000)
    (loop [r (.read raw-r) t (.read text-r) cur cur]
      ;(println (str "cur: "(.toString cur)) (str "r: " (if (> r -1) (char r) r)) (str "t: " (if (> t -1) (char t) t)))
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
  (try
    (println data)
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
      (loop [cur (edn/read {:eof false} r) patterns (sorted-map)
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
                                  (assoc m (str k) (get-val (str v) start end))
                                  )
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
            (seq (map seq patterns))
            ))
        )
      )
    (catch Exception e (println (.getMessage e))))
  )

(defn read-whitespace [cur pos stream whitespace]
  (loop [c cur p pos in-comment (.booleanValue (= c 59))]
    (if (and (> c 0)
             (or (re-matches (re-pattern (str "[,;" whitespace "]")) (str (char c)))
                 in-comment))
      (recur (.read stream) (inc p) (cond (= c 59) true (= c 10) false true in-comment))
      [p c])))
(defn read-token [cur pos stream whitespace list-as-token map-as-token]
  (loop [c cur p pos w "" d 0]
    ;; (if (and (not map-as-token) (> c 0)) (print (char c)))
    (if (and (> c 0)
             (or (> d 0)
                 (re-matches
                  (re-pattern
                   (str "[^" whitespace "]"))
                  (str (char c))))
             (not (and (not map-as-token) (or (= c 123) (= c 125)))))
      (recur (.read stream)
             (inc p)
             (str w (char c))
             (cond (or (= (char c) \[) (and map-as-token (= (char c) \{)) (and list-as-token (= (char c) \())) (inc d)
                   (or (= (char c) \]) (and map-as-token (= (char c) \})) (and list-as-token (= (char c) \)))) (dec d)
                   true d))
      [p w c])))
(defn get-token-map [text source whitespace list-as-token]
  (let [stream (java.io.StringReader. text)]
    (loop [tokens source pos 0 c (.read stream) type nil offsets {} n 0]
      (let [token (first tokens)
            [pos c] (read-whitespace c pos stream whitespace)
            start-pos pos
            [pos word c] (read-token c pos stream whitespace list-as-token true)
            c (.read stream)
            offsets (assoc offsets n [start-pos pos])]
        (if (and (> c 0) (> (count tokens) 0))
          (recur (rest tokens) (inc pos) c type offsets (inc n))
          offsets)
        ))))
(defn get-token-tree [data]
  (let [r (readers/source-logging-push-back-reader data)
        source (edn/read {:eof false} r)
        tree (filter #(not (list? %)) (tree-seq list? identity source))]
    tree
    )
  )
(defn get-annotated-pattern [data]
  (try
    (let [text data
          data (string/replace
                (string/replace data " #(" " \\#(")
                " @" " \\@")
          tree (get-token-tree data)
          tokenize (fn [text source whitespace list-as-token]
                     (let [stream (java.io.StringReader. text)]
                       (loop [tokens source pos 0 c (.read stream) type nil offsets {} n 0]
                         (let [token (first tokens)
                               [pos c] (read-whitespace c pos stream whitespace)
                               start-pos pos
                               [pos word c] (read-token c pos stream whitespace list-as-token true)
                               type (cond (.contains word "scale-p") :scale-p
                                          (.contains word "phrase-p") :phrase-p
                                          (.contains word "drum-p") :drum-p
                                          (.contains word "map-p") :map-p
                                          true type)
                               c (.read stream)
                               offsets (assoc offsets n [start-pos pos])]
                           (if (and (> c 0) (> (count tokens) 0))
                               (recur (rest tokens) (inc pos) c type offsets (inc n))
                             [type offsets])
                           ))))
          [type offset-map] (tokenize text tree "\\s()" false)
          is-rest? (cond (= type :scale-p) #(and (keyword? %) (= \0 (first (name %))))
                         (or (= type :drum-p) (= type :phrase-p)) #(and (keyword? %) (re-find #"^\d" (name %))))
          div (/ 1 (:div (load-string text)))
          is-note? #(and (not (is-rest? %)) (not (and (sequential? %) (keyword? (first %)) (number? (second %)))) (not (= :| %)))
          pattern-idx (+ (first
                          (keep-indexed
                           #(if (.contains (str %2) (name type)) %1) tree))
                         (cond (= type :scale-p) 4
                               (= type :phrase-p) 2
                               (= type :drum-p) 2
                               (= type :map-p) 1
                               true 0))
          rest-regex (cond (= type :scale-p) "0\\\\d+"
                           true "\\\\d+")
          pad (if (or (= :scale-p type) (= :phrase-p type))
                (nth tree (+ pattern-idx 2))
                0)
          pattern (nth tree pattern-idx)
          pattern-pos (get offset-map pattern-idx)
          pattern-offsets (if (map? pattern) {}
                              (second (tokenize (.substring text (inc (first pattern-pos)) (dec (second pattern-pos))) pattern "\\s" true)))
          pattern-map (cond
                        (map? pattern) '()
                        (= type :map-p)
                        (p/build-map-p pattern div)
                        true (p/phrase-p nil pattern div pad []
                                    (fn [n & [n-args]]
                                      (str n))
                                    false is-note? is-rest?
                                    (fn [in a & args]
                                      [(str a)])))
          pattern-map (clojure.walk/prewalk
                       #(if (and (list? %) (= (first %) 'fn)) (str %) %)
                       pattern-map)
          size (if (not (map? pattern)) (p/p-size pattern-map))
          offset-map (if (map? pattern)
                       ;'()
                       (let [stream (java.io.StringReader. (.substring text (inc (first pattern-pos)) (dec (second pattern-pos))))
                             cur (.read stream)
                             whitespace "\\s"]
                         (loop [c cur pos 0 key? true in-bar? true bar nil note nil offsets '()]
                           ;(println "in-bar" in-bar? "key" key? "bar" bar "note" note)
                           (let [[start-pos c] (read-whitespace c pos stream whitespace)
                                 [pos token c] (read-token c start-pos stream whitespace true false)
                                 is-number? (re-matches #"\d+" token)
                                 bar (if (and in-bar? key? is-number?)
                                       (Integer/parseInt token)
                                       bar)
                                 note (if (and (not in-bar?) key? is-number?)
                                       (Integer/parseInt token)
                                       note)
                                 ;x (if (> c 0) (println (char c) start-pos pos))
                                 is-boundary? (or (= c 123) (= c 125))
                                 insert (and (or (not (.endsWith token "}")) (not is-boundary?)) (not key?))
                                 in-bar? (if is-boundary?
                                           (not in-bar?)
                                           in-bar?)
                                 key? (if is-boundary? true (not key?))]
                             ;(println token "insert" insert)
                             (if (> c 0)
                               (recur (.read stream) (inc pos) key? in-bar? bar note
                                      (if insert (conj offsets (list (str [bar note])
                                                                     (list (+ (first pattern-pos) start-pos 2)
                                                                           (+ (first pattern-pos) pos 2)))) offsets))
                               offsets)
                             )))
                       (loop [pos 1 offset-map '() pattern pattern n 0]
                         (let [path (p/get-pos pos (/ 1 div))
                               action (get-in pattern-map path)
                               [pattern n found] (if (and (not (= action [])) (not (nil? action)))
                                                   (loop [p pattern n n]
                                                     (if (and (not (.equals (str action) (str (first p)))) (> (count p) 0))
                                                       (recur (rest p) (inc n))
                                                       [p n true]))
                                                   [pattern n false])
                               offset-map (if found
                                            (conj offset-map
                                                  (list
                                                   (str path)
                                                   (list (+ (first (get pattern-offsets n))
                                                            (first pattern-pos) 2)
                                                         (+ (second (get pattern-offsets n))
                                                            (first pattern-pos) 2))))
                                            offset-map)
                               [pattern n] (if found
                                             [(rest pattern) (inc n)]
                                             [pattern n])]
                           (if (< pos size)
                             (recur (inc pos) offset-map pattern n)
                             offset-map))))]
      ;offset-map
      (conj (conj (conj offset-map (list "pattern" (seq pattern-pos))) (list "div" (int (/ 1 div)))) (list "rest-regex" rest-regex))
      )
    (catch Exception e
      (println (.getMessage e))
      ;(clojure.stacktrace/print-stack-trace e)
      ))
  )

(defn get-step-mode-bounds [pattern]
  (let [bounds (filter #(or (.equals (first %) "pattern") (.equals (first %) "rest-regex") (.equals (first %) "div"))
                       (get-annotated-pattern pattern))
        reg (.replace (second (first (filter #(if (.equals (first %) "rest-regex") (second %)) bounds))) "\\\\" "\\")
        p-pos (second (first (filter #(if (.equals (first %) "pattern") (second %)) bounds)))
        div (second (first (filter #(if (.equals (first %) "div") (second %)) bounds)))
        orig pattern
        pattern (.substring pattern (inc (first p-pos)) (dec (second p-pos)))
        tokens (let [r (java.io.PushbackReader. (java.io.InputStreamReader. (java.io.ByteArrayInputStream. (.getBytes pattern))) )
                          eof (Object.)]
                 (take-while (fn [a] (not= a eof)) (repeatedly (fn [] (read r false eof)))))
        offsets  (get-token-map pattern tokens "\\s" true)]
    (loop [pos 1 idx 0 bounds bounds tokens tokens]
      (let [t (first tokens)
            is-rest (and (keyword? t) (re-matches (re-pattern reg) (name t)))
            offset (get offsets idx)
            offset (if offset [(+ (first offset) (first p-pos) 1) (+ 1 (second offset) (first p-pos))])
            bounds (if (and offset (not (= :| t))) (conj bounds (list (str (p/get-pos pos div)) (seq offset))) bounds)
            pos (if (not (= :| t)) (inc pos) pos)]
        (if (> (count tokens) 0)
          (recur pos (inc idx) bounds (rest tokens))
          bounds)
        )
      )
    )
  )

(defn get-pattern-tbl [& patterns]
  (mapcat
   (fn [p]
     (mapcat
      (fn [[k v]]
        (map
         #(hash-map k (vector (s/to-str (first %)) (second %)))
         (vec (partition 2 v))))
      p))
   patterns)
  )

(defn get-merged-str [& patterns]
  (let [merge-p (if (contains? (first patterns) :div) p/merge-p s/merge-p)
        pp-pattern (if (contains? (first patterns) :div) p/pp-pattern s/pp-pattern)
        pat (apply merge-p patterns)]
    (pp-pattern
     pat)
    )
  )

(defn get-synths []
  (load-synth-descs)
  (let [synths  (filter #(or (= (type %) overtone.studio.inst.Inst)
                             (= (type %) overtone.sc.synth.Synth)
                             (= (type %) techno.synths.Sc-synth)
                             (= overtone.sc.sample.PlayableSample (type %))) (map var-get (vals (ns-publics 'techno.synths))))
        synths (sort (fn [a b] (compare (:name a) (:name b))) synths)
        data (vec (map #(list (:name %)
                                (map (fn [p] (list (:name p) (:default p))) (:params %)))
                       synths))
        c (count data)
        remaining (seq (subvec data (- c (mod c 50))))
        data  (partition 50 data)
        c (inc (int (/ (inc c) 50)))
        data (zipmap (range 1 (inc c)) (if (empty? remaining) data (conj data remaining)))
        data (assoc data "samples" (sort #(compare (first %1) (first %2))
                                         (map #(list (name %)
                                                     (map list (reverse (sort (map name (keys (get drum-kits %))))))
                                                     ) (keys drum-kits))))
        data (assoc data "sketches" (sort #(compare (first %1) (first %2))
                                     (mapcat (fn [[k v]] (if (map? (var-get v)) [(list (str k) '())])) (ns-publics 'techno.sketches2))))]
    (reduce
     #(conj %1 (seq %2))
     '()
     data)
    )
  )

;; (on-event "/n_end"
;;           (fn [info]
;;             (if (contains? @techno.player/node-statuses (first (:args info)))
;;              (let [n (first (:args info))
;;                    k (get @techno.player/node-statuses n)
;;                    nodes (get-in @techno.player/patterns [player k :nodes])]
;;                (swap! techno.player/node-statuses dissoc n)
;;                (swap! techno.player/patterns assoc-in [player k :nodes] (dissoc nodes n))))
;;             )
;;           :node-freed)
;(remove-event-handler :node-freed)
