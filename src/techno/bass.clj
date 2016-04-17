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


(defonce bass-line (atom []))
(swap! bass-line (fn [_]
                   ;; {1 [my-bass [(note :Eb3) :dur 3]]
                   ;;  4 [my-bass [(note :Eb3) :dur 1]]
                   ;;  5 [my-bass [(note :Eb3) :dur 1]]
                   ;;  6 [my-bass [(note :C#3) :dur 3]]
                   ;;  9 [my-bass [(note :C#3) :dur 3]]
                   ;;  12 [my-bass [(note :F3) :dur 1]]
                   ;;  13 [my-bass [(note :Ab3) :dur 1]]
                   ;;  }
                   (mapcat
                    #(repeat 4 [my-bass [(note %) :dur 0.5 :amp 0.2]])
                    [:Eb3 :C#3 :F3 :Ab3]
                    )
                   ))

(comment
  (def bass-player (s/gets 4))
  (s/setsp bass-player 4)
  (s/addp bass-player bass-line)
  (s/rmp bass-player bass-fn)
  (s/addp bass-player get-motif)
  (s/rmp bass-player get-motif)
  (kill bass-player)
  )
