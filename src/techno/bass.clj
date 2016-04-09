(ns techno.bass
  (:use [overtone.core]
        [techno.sequencer :as s])
  )

(defn- map-range [val in out]
  "Maps a value in range of numbers in the first arg to the range in second"
  (let [slope (/
               (- (last out) (first out))
               (- (last in) (first in)))]
    (Math/floor (+ (first out) (* slope (- val (first in)))))
    )
  )

(defsynth bass [note 35 sustain 2 amp 0.4]
  (let [freq (midicps note)
        modulator (/ freq 2)
        sig (pm-osc freq modulator 12)
        env-args [0.1 0 0.5 0.4]
        env (env-gen (apply s/adsr-ng (map * env-args (repeat 4 sustain))) :action 2)]
    (out:ar 0 (* sig env amp))
    (out:ar 1 (* sig env amp))
    )
  )


(def bass-fn (atom #()))
(swap! bass-fn (fn [_]
                 (fn [beat]
                   (let [notes (scale :c3 :lydian)]
                     [bass [(choose notes) :amp 0.1]]
                     )
                   )
                 ))



(def bass-player (s/gcs (dust:kr 5)))
(s/addp bass-player bass-fn)
(s/rmp bass-player bass-fn)
(s/kill-sequencer bass-player)
