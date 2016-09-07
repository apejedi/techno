(ns techno.sketches-live
  (:import java.util.concurrent.ThreadLocalRandom)
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.synths]
        [techno.drum-patterns]
        [techno.drums]))

(def track1
  {:a (fn ([] {:amp 0.2 :dur 1 :vib 0})
        ([amp dur coef]
         (let [defs {:amp 0.2 :dur 1 :coef 0.01}
               amp (if (number? amp) (scale-range amp 0 1 0 0.4) (:amp defs))
               dur (if (number? dur) (scale-range amp 0 1 0 3) (:dur defs))
               c #(chord-degree % :C4 :minor)]
           (s/phrase-p
            sweet
            [(c :vi) (c :v) (c :iv) (c :ii) :3]
            0.25 3 [:amp amp :dur dur]))))

   :a2 (fn ([] {:amp 0.2 :dur 1 :vib 0})
         ([amp dur vib]
          (let [defs {:amp 0.2 :dur 1 :coef 0.01 :vib 0}
                amp (if (number? amp) (scale-range amp 0 1 0 0.4))
                dur (if (number? dur) (scale-range amp 0 1 0 3))
                vib (if (number? vib) vib 0)
                c #(chord-degree % :C4 :minor 4)]
            (s/phrase-p
             sweet
             [(c :v) (c :ii) (c :vi) (c :i) :3]
             0.25 3 [:amp amp :dur dur :vib vib]))))
   :b (fn ([] {:amp 0.4 :coef 0.01 :dur 1 :pushd1 0 :pushd2 0})
        ([amp coef dur]
         (let [root :C4
               type :minor
               coef (if (number? coef) (scale-range coef 0 1 0 0.1))
               amp (if (number? amp) (scale-range amp 0 1 0 0.6))
               dur (if (number? dur) (scale-range amp 0 1 0 3))
               args [:coef coef :amp amp :dur dur]
               inst ks1
               v (flatten (repeat 8 (chord-degree :v root type 4)))
               i (flatten (repeat 8 (chord-degree :iv root type 4)))]
           (s/arp-p inst (concat v i) args 0)
           )))
   :b2 (fn [b]
         (if (= (rand-int 3) 0)
             (let [notes (concat (chord-degree :v :C4 :minor 4) (chord-degree :i :C4 :minor 4))]
               [piano [(choose notes) :dur 3]]
               )))
   :c (s/phrase-p
       overpad
       (concat (chord-degree :v :C4 :minor) [:3] (chord-degree :i :C4 :minor))
       0.25 0 [:dur 1 :amp 0.5]
       {:refresh 1 :reverse 0.3 :sputter 0.8 :sputter-amt 0.2}
       )
   :d (build-from-kits [:Kit10-Vinyl :Kit15-Electro]
                       {
                        1 ["Perc01"]
                        1.5 ["ClHat01"]
                        })
   :e (build-from-kits [:Kit10-Vinyl]
                       {
                        1 ["Perc02"]
                        1.25 ["Perc03"]
                        2 ["Perc04"]
                        2.75 []
                        }
                       )
   :main1 beat
   :main2 (:bomba @beats)
   :main3 (:four-beat @beats)
   })

(def track2
  {:harmony (s/phrase-p
             overpad
             [[:E4 :G4 :B4 :D5] :24
              [:D4 :F4 :A4] :22]
             0.25 0 [:attack 2 :release 5 :amp 0.2])
   :bells (s/phrase-p
           bpfsaw
           [[:D5 :F4] [:E5 :G4] :1 [:D5 :F4] [:E5 :G4] :2
            [:D5 :F4] [:E5 :G4] :3]
           0.25 0 [:amp 0.5 :atk 0.01])
   :whistle {
             1 [whistle [:freq1 (midi->hz (note :D5)) :freq2 (midi->hz (note :A6)) :dur 3 :amp 0.3]]
             8 [whistle [:freq1 (midi->hz (note :A6)) :freq2 (midi->hz (note :E5)) :dur 3 :amp 0.3]]
             12.75 []}
   :kicks (let [k "Kick-01" s "Snr03"]
            (build-from-kits
             [:Kit3-Acoustic :Kit16-Electro]
             [[k [dub-kick [100 :amp 1]]] :3]
             0.25))
   :sdst (let [c1 "ClHat02" c2 "ClHat01" a [:amp 0]]
           (build-from-kits
            [:Kit16-Electro]
            [[c1] :2 [c1] :2 [c1] :1 [c1] [c2] [c1] :5]
            0.25))
   :hat (let [o "OpHat"]
            (drum-pattern
             [:Kit16-Electro]
             [:2 [o] :1]
             0.25))
   :sdst2 (let [s3 "SdSt-03"
                s6 "SdSt-06"
                s7 "SdSt-07"
                s4 "SdSt-04"]
           (drum-pattern
            [:Kit3-Acoustic]
            [[s6] :2 [s6] :2 [s6] :1 [s6] [s7] [s6] :5]
            0.25 [:amp 0.6]))
   ;; :f (let []
   ;;      (s/phrase-p
   ;;       plk-bass
   ;;       [:D3 :G3 :C3 :A3]
   ;;       0.25 4 [:dur 2]))
   }
  )

(def house2
  {:harmony (s/phrase-p
             rise-fall-pad
             [(map midi->hz (chord :C3 :M7)) (map midi->hz (chord :F3 :M7)) :34]
             0.25 32 [:t 5])
   :motif (s/phrase-p
           bass-synth
           [[:E3 :B3] [:A3 :F3] :34]
           0.25 32 [:release 6 :amp 0.4 :detune 4])
   :motif2 (s/phrase-p
           bass-synth
           [[:E4 :B3] [:A4 [:amp 0.3] :F4 [:amp 0.3]] :34]
           0.25 32 [:release 6 :amp 0.4 :detune 4])
   :kick (drum-pattern
          [:Kit4-Electro :Kit3-Acoustic]
          [[k1] :1 [o1] :1]
          0.25)
   :cl (drum-pattern
        [:Kit16-Electro]
        [[c1] [c1] [c2] :2 [c1] :2]
        0.25 [:amp 0.3])
   :snr (drum-pattern
         [:Kit3-Acoustic :Kit16-Electro :Kit4-Electro]
         [:2 [s3] :1]
         0.25)
   :t (drum-pattern
       [:Kit5-Electro]
       [[cl1] :2]
       0.25)
   })
