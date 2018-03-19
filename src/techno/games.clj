(ns techno.games
  (:import java.util.concurrent.ThreadLocalRandom)
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.player :as p]
        [techno.synths]
        [techno.samples]
        [techno.drum-patterns]
        )
  )
(defn chord-changes
  ([] (chord-changes :F4))
  ([root]
   (let [degrees (atom (techno.sequencer/x-seq [:i :ii :iii :iv :v :vi]))
          mk-change (fn []
                      (let [a (first @degrees)
                            x (reset! degrees (rest @degrees))
                            b (first @degrees)
                            x (reset! degrees (rest @degrees))]
                        [a b]))
          [a b] (mk-change)
          reader (java.io.BufferedReader. *in*)]
      (loop [a a b b arpeggiate false]
        (let [[c d] (mk-change)
              pat (p/phrase-p piano (if arpeggiate
                                      (flatten
                                       [(chord-degree a root :major 3) :|
                                        (chord-degree b root :major 3)])
                                      [(chord-degree a root :major 3) :|
                                       (chord-degree b root :major 3)])
                              1/4 0 [:dur 2])]
          (p/play-p pat)
          (let [prog (.readLine reader)
                correct (.equals prog (str a " " b))
                next  (.equals "n" prog)
                quit (.equals "q" prog)
                repeat (.equals "r" prog)
                show (.equals "s" prog)]
            (cond
              (or next quit show) (println a b)
              correct (println "yep" a b)
              true (println "nope"))
            (if (not quit)
              (recur
               (if next c a)
               (if next d b)
               (.equals "a" prog)
               )))
          )
        )))
  )
