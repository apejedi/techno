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


(def marimba (atom nil))
(swap! marimba
       (fn [_]
         (fn [b]
           (let [;cfmin (* (choose (map midi->hz (scale :C4 :major))) (choose [0.5 1 2 4]))
                 cfmin (* (midi->hz 64) (choose [0.5 1 2 4]))]
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
                       chord (map midi->hz (choose [[23 35 54 63 64]
                                                    [45 52 54 59 61 64]
                                                    [28 40 47 56 59 63]
                                                    [42 52 57 61 63]]))]
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
                                   :amp 0.5])
                       ))
                 )))
(comment
  (s/add-p core/player marimba :marimba)
  (s/add-p core/player drone :drone)
  )
