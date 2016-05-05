(ns techno.bass
  (:use [overtone.core]
        [overtone.inst.synth]
        [techno.sequencer :as s]
        [techno.motifs]
        [techno.recorder :as rec])
  )

(defn- map-range [val in out]
  "Maps a value in range of numbers in the first arg to the range in second"
  (let [slope (/
               (- (last out) (first out))
               (- (last in) (first in)))]
    (Math/floor (+ (first out) (* slope (- val (first in)))))
    )
  )

(defsynth my-bass [note 35 dur 2 amp 0.4]
  (let [freq (midicps note)
        modulator (/ freq 2)
        sig (pm-osc freq modulator 12)
        env-args [0.1 0.5 0.2 0.2]
        env (env-gen (apply s/adsr-ng (map * env-args (repeat 4 dur))) :action 2)]
    (out:ar 0 (* sig env amp))
    (out:ar 1 (* sig env amp))
    )
  )
(defsynth grit-bass [note 35 dur 2 amp 0.4]
  (let [freq (midicps note)
        modulator (* freq 1.5)
        sig (pm-osc freq modulator 10)
        env-args [0.1 0.5 0.2 0.2] ;a d s r
        env (env-gen (apply s/adsr-ng (map * env-args (repeat 4 dur))) :action 2)]
    (out:ar 0 (* sig env amp))
    (out:ar 1 (* sig env amp))
    )
  )


(defonce bass-line (atom []))
(swap! bass-line
       ;; (fn [_]
       ;;   (mapcat
       ;;    #(repeat 2 [my-bass [(note %) :dur 0.6 :amp 0.05]])
       ;;    [:Ab2 :C2 :G2 :D2 :Bb2]
       ;;    )
       ;;   )
       (fn [_]
         {
          1 [bass [(midi->hz (note :C4)) 0.1]]
          1.25 [bass [(midi->hz (note :D4)) 0.1]]
          1.75 []
          2 [bass [(midi->hz (note :D4)) 0.1]]
          2.25 [ bass [(midi->hz (note :C4)) 0.1]]
          2.75 []
          }
         )
       )

(comment
  (def beat (get-time-pattern))
  (my-bass (note :D4) 0.4)
  (def bass-player (s/get-s 2 0.25))
  (s/set-sp bass-player 2)
  (s/set-st bass-player 0.25)
  (s/add-p bass-player bass-line)
  (s/rmp bass-player bass-fn)
  (s/addp bass-player get-motif)
  (s/rmp bass-player get-motif)
  (kill bass-player)
  )
