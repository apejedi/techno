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
(defn chord-changes []
  (let [;root (choose [:A3 :Bb3 :B3 :C3 :C#3 :D3 :Eb3 :E3 :F3 :F#3 :G3 :Ab3 :A3 :Bb3 :B3])
        root :C3
        degrees (atom (techno.sequencer/x-seq [:i :ii :iii :iv :v :vi]))
        mk-change (fn []
                    (let [a (first @degrees)
                          x (reset! degrees (rest @degrees))
                          b (first @degrees)
                          x (reset! degrees (rest @degrees))]
                      [a b]))
        [a b] (mk-change)]
    (loop [a a b b]
      (let [[c d] (mk-change)
            pat (p/phrase-p piano [(chord-degree a root :major) :|
                                   (chord-degree b root :major)]
                                  1/4 0 [:dur 2])]
        (p/play-p pat)
        (let [prog (read-line)
              correct (.equals prog (str a " " b))
              next (.equals "n" prog)
              quit (.equals "q" prog)]
          (cond
            (or next quit) (println a b)
            correct (println "yep")
            true (println "nope"))
          (if (not quit)
            (recur
             (if next c a)
             (if next d b)
               )))
        )
        ))
  )
