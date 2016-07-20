(ns techno.bass
  (:use [overtone.core]
        [techno.synths]
        [techno.core :as core]
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
        modulator (* freq 2)
        sig (pm-osc freq modulator 12)
        sig (lpf sig freq)
        env-args [0.01 0.5 0.2 0.2]
        env (env-gen (apply s/adsr-ng (map * env-args (repeat 4 dur))) :action 2)]
    (out:ar 0 (* sig env amp))
    (out:ar 1 (* sig env amp))
    )
  )

(defsynth grit-bass [note 35 dur 2 amp 0.4]
  (let [freq (midicps note)
        modulator (* freq 1.5)
        sig (rlpf (pm-osc freq modulator 10) freq)
        env-args [0.01 0.5 0.2 0.2] ;a d s r
        env (env-gen (apply s/adsr-ng (map * env-args (repeat 4 dur))) :action 2)]
    (out:ar 0 (* sig env amp))
    (out:ar 1 (* sig env amp))
    )
  )





(defonce bass-line (atom []))
(swap! bass-line
       (fn [_]
         (s/phrase-p
          grit-bass
          [:G3 :A3 :C3]
          0.25
          3
          [:dur 3 :amp 1])
          )
       )

(defonce bass-pulse (atom []))
(swap! bass-pulse
       (fn [_]
         (s/phrase-p
          plk-bass
          [:C3 :1 :D2 :F#3]
          0.25
          0
          [:amp 0.7 :t 1.5])
         )
       )



(comment
  (my-bass (choose (scale :C3 :major)) :amp 1)
  (bass (midi->hz (note :C3)))
  (stop)
  (s/play-p bass-pulse 2)
  (vintage-bass (choose (scale :C3 :major)) 50)
  (s/add-p core/player bass-pulse :bass)
  (s/add-p core/player bass-line :bass)
  (s/rm-p core/player :bass)
  (s/wrap-p core/player :bass)
  (s/rm-p core/player :bass)
  (s/add-p melissa :bass)
  )
