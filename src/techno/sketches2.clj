(ns techno.sketches2
  (:import java.util.concurrent.ThreadLocalRandom)
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.player :as p]
        [techno.synths]))

(comment
  (def player (get-s 95))
  (stop-s player)
  (p/add-p player (:klang bells) :klang)
  (p/add-p player (:bpfsaw bells) :sin)
  (p/add-p player (:sin bells) :s)
  (p/add-p player (:bowed bells) :bowed)
  )

(def bells
  {:klang (p/phrase-p
            klang-test
            [:G3  :F#4  :F#4  :G3  :F#4  :F#4  :G3  :F#4  :A3  :G4  :G4  :A3  :G4  :G4  :A3  :G4  :B3  :B4  :B4  :B3  :B4  :B4  :B3  :B4  :C#4  :A4  :A4  :C#4  :A4  :A4
             :C#4  :A4]
            0.25 1 [:atk 0.01 :dur 1.6])
   :sin (p/phrase-p
            sin-inst
            [:G4  :D5  :G4 :D5  :G4  :D5  :G4  :D5  :A4  :E5  :A4  :E5  :A4  :E5  :A4  :E5  :F#4  :E5  :F#4  :E5  :F#4  :E5  :F#4  :E5  :F#4  :D5  :F#4 :D5  :F#4 :D5  :F#4  :D5]
            0.25 1 [:dur 0.7 :amp 0.5])
   :bpfsaw (p/phrase-p
            bpfsaw
            [:G4 :B4  :A5  :G4  :B4  :G5 :G4 :B4  :F#5  :G4  :B4  :E5  :F#4  :A4  :D5 :3]
            0.25 1 [:dur 1.25 :atk 0.01])
   :flute (p/phrase-p
           flute
           [:G4 :G4 :G4 :12 :E5 :E5 :4 :D5 :D5 :8 :A4 :1 :A4 :1 :A4 :10 :B4 :1 :B4 :B4 :8]
           0.25 0 [])
   :bowed (p/phrase-p
           bowed
           [[:A3 :D4 :A4] :A3 [:A4 :D4] :1 [:A3 :A4] :1 :A3 :A3 [:A4 :D4] [:A3 :D4] :1 :A3 :D4 :1 [:D4 :A4 :A3] :1 [:D4 :A3 :A4] :1 [:A3 :D4 :A4] :1 [:A3 :D4 :A4] :1 [:A3 :D4 :A4] :1 [:A3 :D4 :A4] :A3 :D4 [:A3 :D4 :A4] :3 [:G4 :C4 :G3] :1 [:G3 :C4 :G4] :1 [:G4 :G3 :C4] :1 [:C4 :G4 :G3] :1 [:C4 :G4 :G3] :1 [:C4 :G3 :G4] :1 [:G3 :C4 :G4] :1 [:C4 :G3 :G4] :1 [:C4 :G3 :G4] :1 [:G4 :C4] :G3 [:C4 :G4 :G3] :1 [:C4 :G4 :G3] :1 [:C4 :G3 :G4] :1 [:C4 :G3] :1 [:G4 :C4 :G3] :3 [:A3 :D3] :1 [:A3 :D3] :A3 :D3 [:A3 :D3] :1 :A3 :1 [:A3 :D3] :1 [:A3 :D3] :D3 :42]
           0.25 0 [:amp 0.07])
   })
