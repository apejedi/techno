(ns techno.ambient
  (:import java.util.concurrent.ThreadLocalRandom)
  (:use [overtone.core]
        [techno.core :as core]
        [techno.sequencer :as s]
        [techno.synths]))




(def marimba (atom nil))
(swap! marimba
       (fn [_]
         (fn [b]
           (let [cfmin  (choose (map midi->hz (scale :C5 :major)))
                ; cfmin (* (midi->hz 64) (choose [0.5 1 2 4]))
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
                 :amp 5]]))
           )))

(def drone (atom nil))
(swap! drone (fn [_]
               (fn [b]
                 (let [rand #(.nextDouble (ThreadLocalRandom/current) %1 %2)
                       ;chord (map midi->hz (chord-degree (choose [:ii :iv :v :vi]) :A4 :minor))
                       chord
                       (map midi->hz (chord-degree (choose [:i :ii :iii :iv :v]) :C4 :major))

                       ;; (map #(midi->hz (note %))
                       ;;            (choose
                       ;;             [[:B0 :B1 :F#3 :Eb4 :E4]
                       ;;              [:A2 :E3 :F#3 :B3 :C#4 :E4]
                       ;;              [:E1 :E2 :B2 :Ab3 :B3 :Eb4]
                       ;;              [:F#2 :E3 :A3 :C#4 :Eb4]]
                       ;;             ))
                       ]
                   (if (and (integer? b) (= (rand-int 2) 0))
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
                                   :amp 1.3])
                       ))
                 )))


(def spooky-bells
  (fn [b]
    (let [n (midi->hz (note (choose
                             (scale :C4 :major)
                             ;; [:B5 :B4 :F#3 :Eb4 :E4
                             ;;         :A4 :E5 :F#4 :B5 :C#4 :E4
                             ;;         :E5 :E4 :B5 :Ab4 :B4 :Eb4
                             ;;         :F#5 :E4 :A4 :C#4 :Eb4]
                             )))]
      (if (and
           (or (integer? b))
           (= (rand-int 3) 0))
        [klang-test [n :amp (rand 0.6) :atk 0.01]]
        ))
    ))
(comment
  (s/add-p core/player marimba :marimba)
  (s/add-p core/player drone :drone)
  (s/add-p core/player spooky-bells :spooky)
  (s/rm-p core/player :drone)
  (s/play-p  [(s/chord-p klang-test  (map midi->hz (chord-degree (choose [:i :ii :iii :iv :v]) :C4 :minor 2)))])
  )
