(ns techno.drum-patterns
  (:use [techno.drums]
        [techno.sequencer :as s :exclude [t]]
        [techno.core :as core]
        [techno.synths]
        [overtone.inst.synth]
        [techno.samples]
        [techno.recorder]
        )
  )

(defmacro drum-pattern [kits pattern & [step args]]
  (list 'let [['t1 't2 't3 't4 't5 't6] (list 'map #(str "Tom" %) (list 'range 1 7))
              ['k1 'k2 'k3 'k4 'k5 'k6] (list 'map #(str "Kick" %) (list 'range 1 7))
              ['c1 'c2 'c3 'c4 'c5 'c6] (list 'map #(str "ClHat" %) (list 'range 1 7))
              ['cl1 'cl2 'cl3 'cl4 'cl5 'cl6] (list 'map #(str "Clap" %) (list 'range 1 7))
              ['cy1 'cy2 'cy3 'cy4 'cy5 'cy6] (list 'map #(str "Cymbal" %) (list 'range 1 7))
              ['cr1 'cr2 'cr3 'cr4 'cr5 'cr6 'cr7] (list 'map #(str "Crash" %) (list 'range 1 8))
              ['r1 'r2 'r3 'r4 'r5 'r6] (list 'map #(str "Rim" %) (list 'range 1 7))
              ['ri1 'ri2 'ri3 'ri4 'ri5 'ri6] (list 'map #(str "Ride" %) (list 'range 1 7))
              ['h1 'h2 'h3 'h4 'h5 'h6] (list 'map #(str "HfHat" %) (list 'range 1 7))
              ['f1 'f2 'f3 'f4 'f5 'f6] (list 'map #(str "fx" %) (list 'range 1 7))
              ['o1 'o2 'o3 'o4 'o5 'o6] (list 'map #(str "OpHat" %) (list 'range 1 7))
              ['sd1 'sd2 'sd3 'sd4 'sd5 'sd6 'sd7 'sd8 'sd9 'sd10] (list 'map #(str "SdSt" %) (list 'range 1 11))
              ['s1 's2 's3 's4 's5 's6 's7 's8 's9 's10 's11 's12] (list 'map #(str "Snr" %) (list 'range 1 13))]
        (list 'build-from-kits
              kits
              pattern
              (if step step 0.25)
              (if args args []))
        )
  )

(defonce there-there (atom nil))
(swap! there-there
       (fn [_]
         (build-from-kits
          [:Kit3-Acoustic]
          [["Kick-02"] :3
           ["Tom-01" "SdSt-07"] :1
           ["Tom-04"] :2
           ["SdSt-04" "SdSt-03"]
           ["SdSt-07" "SdSt-05"] :3
           ]
          )
         ))



(def boc-beat
  (build-from-kits
   [:Kit3-Acoustic :Kit10-Vinyl]
   {1 ["Tom-01" [dub-kick [200]]]
    1.75 ["Tom-04" [dub-kick [150]]]
    2 ["Tom-05" [dub-kick [100]]]
    2.25 ["Tom-04" [dub-kick [200]] "Rim-01"]
    3 ["Tom-01" [dub-kick [200]]]
    3.5 ["Tom-04" [dub-kick [150]]]
    4 ["Tom-05" "Snr-04" [dub-kick []] "SdSt-07"]
    4.75 []
    }
   )
  )

(def t (atom nil))
(swap! t
       (fn [_]
         (let [a [:amp 0.4]
               base (build-from-kits
                     [:Kit3-Acoustic]
                     [["SdSt-03"] :2
                      ["SdSt-06"] ["SdSt-07"] :1 ["SdSt-03" "Snr-04"] :3 ;:1
                      ["SdSt-05" "Snr-09"] :1
                      ["Snr-04" "SdSt-07"] :3]
                     0.25 [:amp 0.4]
                     )]
           (s/m-phrase
            {:refresh 0 :sputter 0.5 :sputter-amt 0.3 :reverse 0.5}
             base 0.25))
         ))


(def lazer (let [d [zap [3000 :amp 0.3 :dur 0.1] dub-kick [300 :amp 1.5]]
                 base (s/build-rest-p
               [d :6 d :2 d :5])]
             (s/m-phrase
              {:refresh 0.7 :sputter 0.7 :sputter-amt 0.3}
              base
              0.25)
             ))
(comment
  (s/add-p core/player there-there :main3)
  (s/set-st core/player (double (/ 1 8)))
  (s/add-p core/player boc-beat :main)
  (s/add-p core/player lazer :pulse)
  (s/add-p core/player untitled-b :switch)
  (s/add-p core/player t :t)
  (start-recorder (mapcat vals
                          (vals (group-samples (drum-kits :Kit16-Electro)))))




  (let [kick (drum-pattern
              [:Kit4-Electro :Kit3-Acoustic]
              [[k1] :1 [o1]]
              0.25)
        cl (drum-pattern
            [:Kit16-Electro]
            [[c1] [c1] [c2] :2 [c1] :2]
            0.25)
        snr (drum-pattern
            [:Kit3-Acoustic :Kit16-Electro]
            [:6 [cl1] :1]
            0.25)
        t (drum-pattern
           [:Kit5-Electro]
            [:2 [[bing []]]]
            0.25)]
    (s/add-p core/player kick :kick)
    (s/add-p core/player cl :cl)
    ;(s/pp-pattern (s/merge-p kick cl))
    (s/add-p core/player snr :snr)
    (s/add-p core/player t :fx)
    (s/play-p kick cl snr 1.6)
    )

  (s/play-p techno1 funky-drummer 2)
  (s/add-p core/player techno1 :main3)
  (s/add-p core/player
           funky-drummer :main2)
  (s/add-p core/player (s/m-phrase {:refresh 0.8 :sputter 0.7 :sputter-amt 0.3}
                                   funky-drummer 0.25) :main2)
  (s/add-p core/player impeach-the-president :main1)
  (s/play-p impeach-the-president 2)


  (s/rm-p core/player :sd)
  (s/wrap-p core/player :pulse false)
  )

(def funky-drummer
  (let [k "Kick-03" s "Snr-04" c "ClHat-04" o "OpHat-01"]
    (build-from-kits
     [:Kit3-Acoustic]
     [[k c] [c] [k c] [c] [s c] [c]
      [k c] [s o] [c] [s c] [k c] [s c] [s c]
      [k o] [c] [s c]]
     0.25 [:amp 0.5])))

(def techno1
  (let [k "Kick02"
        s "Snr03" c "ClHat02" o "OpHat" cl "Clap02"]
    (build-from-kits
     [:Kit16-Electro]
     [[k] :1 [o] :1 [k s] :1 [o] :1
      [k] [c] [o] :1 [k s] :1 [k o] :1]
     0.25 [:amp 0.5])))

(def son-clave
  (let [k "Kick-03" r "Rim" c "ClHat"]
    (build-from-kits
     [:Kit3-Acoustic :Kit11-Vinyl]
     [[k r c] [c] [c] [k r c]
      [k c] [c] [r c] [k c] [k c]
      [c] [r c] [k c] [k r c] [c]
      [c] [k c]
      ])))

(def bossa-nova
  (let [k "Kick-03" r "Rim" c "Ride02"]
    (build-from-kits
     [:Kit3-Acoustic :Kit11-Vinyl]
     [[k r c] [c] [c] [k r c]
      [k c] [c] [r c] [k c] [k c]
      [c] [r c] [k c] [k c] [r c]
      [c] [k c]
      ])))

(def impeach-the-president
  (let [k "Kick-03" s "Snr-04" c "ClHat-04" o "OpHat-01"]
    (build-from-kits
     [:Kit3-Acoustic]
     [[k c] :1 [c] :1 [s c] :1 [c]
      [k c] [k c] :1 [o] :1 [s c] :1 [k c] :1]
     0.125 [:amp 0.5])))
