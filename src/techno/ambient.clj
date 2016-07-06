(ns techno.ambient
  (:import java.util.concurrent.ThreadLocalRandom)
  (:use [overtone.core]
        [techno.core :as core]
        [techno.sequencer :as s]))


(defsynth bpfsaw2 [freq 500 atk 2 sus 0 rel 3 c1 1 c2 -1
		 detune 0.2 pan 0 cfhzmin 0.1 cfhzmax 0.3
		cfmin 500 cfmax 2000 rqmin 0.1 rqmax 0.2
                   lsf 200 ldb 0 amp 1 output 0]
  (let [env (env-gen:kr (envelope [0 1 1 0] [atk sus rel] [c1 0 c2]) :action 2)
        f (* freq (midiratio (* (lf-noise0:kr 0.5) detune)))
        sig (saw [f f])
        noise (lin-exp
               (lf-noise1:kr
                (lin-exp (lf-noise1:kr 4) -1 1 cfhzmin cfhzmax))
               -1 1 cfmin cfmax)
        sig (bpf sig noise (lin-exp (lf-noise1:kr 0.1) -1 1 rqmin rqmax))
        sig (b-low-shelf sig lsf 0.5 ldb)
        sig (balance2 (first sig) (second sig))
        sig (* sig env amp)]
    (out output sig)
    ))

(defsynth klang-test [freq 440 amp 1 atk 0.1 dur 3]
  (let [partials (map double
                      ;[1]
                      [(/ 1 2) (/ 2 3) 1 (/ 4 3) 2 (/ 5 2)]
                      )
        num (count partials)
        sig (klang [(map
                     #(* freq %)
                     ;#(lin-exp (lf-noise1:kr 0.001) -1 1 (* freq %) (* freq % 2))
                     partials)
                    (repeat num (double (/ 1 num)))
                    ])
        env (env-gen (perc (* atk dur) (* (- 1 atk) dur)) :action FREE)
        sig (* sig env amp)]
    (out 0 [sig sig])
    )
  )


(def marimba (atom nil))
(swap! marimba
       (fn [_]
         (fn [b]
           (let [;cfmin (* (choose (map midi->hz (scale :C4 :minor))) (choose [0.5 1 2 4]))
                 cfmin (* (midi->hz 64) (choose [0.5 1 2 4]))
                 ]
             (if (= (rand-int 2) 1)
               [bpfsaw2
                [:dur (choose [1 0.5])
                 :freq (choose (map double [(/ 1 2) (/ 2 3) 1 (/ 4 3) 2 (/ 5 2) 3 4 6 8]))
                 :detune (rand 0.1)
                 :rqmin 0.005
                 :rqmax 0.008
                 :cfmin cfmin
                 :cfmax (* cfmin (choose (range 1.008 1.025 0.001)))
                 :atk 3
                 :sus 1
                 :rel 5
                 :amp 1]]))
           )))

(def drone (atom nil))
(swap! drone (fn [_]
               (fn [b]
                 (let [rand #(.nextDouble (ThreadLocalRandom/current) %1 %2)
                       ;chord (map midi->hz (chord-degree (choose [:ii :iv :v :vi]) :A4 :minor))
                       chord
                       ;(map midi->hz (chord-degree (choose [:i :ii :iii :iv :v]) :C4 :minor))

                       (map #(midi->hz (note %))
                                  (choose
                                   [[:B0 :B1 :F#3 :Eb4 :E4]
                                    [:A2 :E3 :F#3 :B3 :C#4 :E4]
                                    [:E1 :E2 :B2 :Ab3 :B3 :Eb4]
                                    [:F#2 :E3 :A3 :C#4 :Eb4]]
                                   ))
                       ]
                   (if (and (integer? b) (= (rand-int 6) 0))
                       (s/chord-p bpfsaw2
                                  chord
                                  [:dur (rand 1.5 4.0)
                                   :detune (rand 0.05 0.1)
                                   :cfmin 100
                                   :cfmax 1500
                                   :rqmin (rand 0.01 0.15)
                                   :atk (rand 2.0 2.5)
                                   :rel (rand 6.5 10.0)
                                   :ldb 6
                                   :amp 0.3])
                       ))
                 )))


(def spooky-bells
  (fn [b]
    (if (and
         (or (integer? b))
         (= (rand-int 4) 0))
      [klang-test [(midi->hz (choose (scale :C5 :minor))) :amp (rand 0.3) :atk 0.0001]]
      )
    ))
(comment
  (s/add-p core/player marimba :marimba)
  (s/add-p core/player drone :drone)
  (s/add-p core/player spooky-bells :spooky)
  (s/play-p  [(s/chord-p klang-test  (map midi->hz (chord-degree (choose [:i :ii :iii :iv :v]) :C4 :minor 2)))])
  )
