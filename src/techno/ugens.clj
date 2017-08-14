(ns techno.ugens
  (:use [techno.ugen-util])
  )
(def specs
  [{:name "DWGBowedTor",
    :args [{:name "freq",
            :default 440.0
            :doc "Frequency in Hertz"}
           {:name "velb"
            :default 0.5
            :doc "Bow Velocity"}
           {:name "force"
            :default 1
            :doc "Bow normal force"}
           {:name "gate"
            :default 1
            :doc "Releases synth when value changes from >0 to 0."}
           {:name "pos"
            :default 0.14
            :doc "Relative bow position from 0 to 1."}
           {:name "release"
            :default 0.1
            :doc "Release time in seconds."}
           {:name "c1"
            :default 1
            :doc "Inverse of DC decay time."}
           {:name "c3"
            :default 3
            :doc "Hight frequency loss factor."}
           {:name "impZ"
            :default 0.55
            :doc "String impedance."}
           {:name "fB"
            :default 2
            :doc "Inharmonicity factor."}
           {:name "mistune"
            :default 5.2
            :doc "The relative frequency of torsional waves."}
           {:name "c1tor"
            :default 1
            :doc "Same as c1 for torsional waves."}
           {:name "c3tor"
            :default 3000
            :doc "Same as c3 for torsional waves."}
           {:name "iZtor"
            :default 1.8
            :doc "Torsional waves string impedance."}]
    :rates #{:ar}
    :summary "Digital wave guide physical model of a bowed instrument. Sound must go throught BowSoundBoard for better sound. Like DWGBowed but also with torsional waves"
    }
   {:name "DWGSoundBoard"
    :args [{:name "inp"
            :default 0}
           {:name "c1"
            :default 20}
           {:name "c3"
            :default 20}
           {:name "mix"
            :default 0.8}
           {:name "d1"
            :default 199}
           {:name "d2"
            :default 211}
           {:name "d3"
            :default 223}
           {:name "d4"
            :default 227}
           {:name "d5"
            :default 229}
           {:name "d6"
            :default 233}
           {:name "d7"
            :default 239}
           {:name "d8"
            :default 241}]
    :rates #{:ar}
    :summary "DWG Sound Board wut"
    }])

(doseq [spec specs]
  (def-ugen 'overtone.core spec 0)
  )
