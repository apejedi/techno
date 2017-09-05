(ns techno.sketches2
  (:import java.util.concurrent.ThreadLocalRandom)
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.player :as p]
        [techno.synths]
        [techno.samples]
        [techno.drum-patterns]
        )
  )

(comment
  (def player (get-s 80))
  (def player 21)
  (stop-s player)
  (p/set-sp player 90)
  (p/rm-p player :hats)
  (let [patterns {:kick (p/drums
                         [:Kit4-Electro :Kit10-Vinyl]
                         [:k1 :|]
                         1/8)
                  :hats (p/drums
                         [:Kit10-Vinyl]
                         [:o1 :| :c1 :c2 :1 :c1 :|
                          :c2 :c1 :c1 :|]
                         1/4)
                  :clap (p/drums
                         [:Kit15-Electro]
                         [:cl1 :| :|]
                         1/4)
                  :bass (p/phrase-p
                         acid-bass
                         [:C2 :3 :A1 :1 :B1 :| :| :|]
                         1/8 0 [:dur 0.4])
                  }]
    (p/rm-p player :all)
    (doseq [[k p] patterns]
      (p/add-p player p k)
      )
    )

  )

(def template
  {:drum1 (drum-p2 [:Kit15-Electro] [])
   :drum2 (drum-p2 [:Kit16-Electro] [])
   :drum3 (drum-p2 [:Kit9-Vinyl] [])
   :drum4 (drum-p2 [:KurzweilKit08] [])
   :motif1 (p/phrase-p
            bpfsaw
            []
            0.25 0 [])
   :motif2 (p/phrase-p
            overpad
            []
            0.25 0 [])
   :motif3 (p/phrase-p
            bass2
            []
            0.25 0 [])
   :motif4 (p/phrase-p
            plk-bass
            []
            0.25 0 [])
   :motif5 (p/phrase-p
            bpfsaw
            []
            0.25 0 [])
   :harmony1 (p/phrase-p
            bpfsaw
            []
            0.25 0 [])
   :harmony2 (p/phrase-p
            overpad
            []
            0.25 0 [])
   :harmony3 (p/phrase-p
            bass2
            []
            0.25 0 [])
   :harmony4 (p/phrase-p
            plk-bass
            []
            0.25 0 [])
   :harmony5 (p/phrase-p
            bpfsaw
            []
            0.25 0 [])

   }
  )

(def sketch
  {:flute (p/phrase-p
           bass-synth
           [:A2 :| :| :D3 :| :| :G3 :| :| :D3 :| :| :A2 :| :| :| :|]
           1/4 0 [:attack 1 :release 4 :bwr 1])

   :klang (p/phrase-p
           reverb-test
           [:A3 :8 :D4 :8 :G4 :| :| :| :|]
           1/4 0 [:decay 5.354330708661418])
   :sin (p/phrase-p
         rise-pad
         [[:G4 :F3 :D4] :12 [:E4 :B4 :G3] :12 [:C4 :A3 :G4 :D3] :| :| :| :| :| :|]
         1/4 0 [:t 4])
   :bpfsaw (p/phrase-p
            bing
            [:C3 :G3 :D4 :B4 :| :|]
            1/4 0 [:decay 1.8425197582545243])

   })
