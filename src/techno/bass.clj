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
       (fn [_]
         (mapcat
          #(repeat 2 [my-bass [(note %) :dur 0.6 :amp 0.05]])
          [:Ab2 :C2 :G2 :D2 :Bb2]
          )
         )
       ;; (fn [_]
       ;;   (let [dur 2 amp 0.05]
       ;;     {2 [my-bass [(note :Ab2) dur amp]]
       ;;      4 [my-bass [(note :C2) dur amp]]
       ;;      6 [my-bass [(note :G2) dur amp]]
       ;;      8 [my-bass [(note :D2) dur amp]]
       ;;      10 [my-bass [(note :Bb2) dur amp]]
       ;;      }
       ;;     ))
       )

(comment
  (my-bass (note :C4))
  (def bass-player (s/gets 4))
  (s/setsp bass-player 4)
  (s/addp bass-player bass-line)
  (s/rmp bass-player bass-fn)
  (s/addp bass-player get-motif)
  (s/rmp bass-player get-motif)
  (kill bass-player)
  )
