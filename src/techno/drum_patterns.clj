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
         (let [a [:amp 0.4]]
           (build-from-kits
            [:Kit3-Acoustic]
            [["SdSt-03"] :2
             ["SdSt-06"] ["SdSt-07"] :1 ["SdSt-03" "Snr-04"] :1 ;:3
             ;; ["SdSt-05" "Snr-09"] :1
             ;; ["Snr-04" "SdSt-07"] :3
             ]
            0.25 [:amp 0.4]))
         ))


(def lazer (let [d [zap [:amp 0.1] dub-kick [300 :amp 1.5]]]
             (s/build-rest-p
              [d :6 d :2 d :5])
             ))
(comment
  (s/add-p core/player there-there :main3)
  (s/set-st core/player (double (/ 1 8)))
  (s/add-p core/player boc-beat :main)
  (s/add-p core/player lazer :pulse)
  (s/add-p core/player untitled-b :switch)
  (s/play-p funky-drummer son-clave techno1 2)
  (s/add-p core/player t :t)

  (s/add-p core/player techno1 :main3)
  (s/add-p core/player funky-drummer :main)
  (s/add-p core/player son-clave :main)
  (s/play-p
   (build-from-kits
    [:Kit3-Acoustic]
    ["SdSt-07" "SdSt"]
    )
   )

  (s/rm-p core/player :main2)
  (s/wrap-p core/player :pulse false)
  (s/add-p core/player
   (build-from-kits
    [:Kit10-Vinyl]
    [[[kick [:sustain 1 :noise 0.5 :amp 0.2]]] :7]
    0.25) ;2
    :main2)
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
  (let [k "Kick-03" s "Snr-04" c "ClHat-04" o "OpHat-01"]
    (build-from-kits
     [:Kit3-Acoustic]
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
     0.25 [:amp 0.5])))
