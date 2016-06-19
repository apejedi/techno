(ns techno.bass
  (:use [overtone.core]
        [overtone.inst.synth]
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
        modulator (/ freq 2)
        sig (pm-osc freq modulator 12)
        sig (lpf sig freq)
        env-args [0.1 0.5 0.2 0.2]
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
          vintage-bass
          [:E3 :D3 :C3 :B3 [:space 0]]
          (double (/ 1 4))
          0
          [:dur 1 :amp 0.3]
          ))
       )

(defonce bass-pulse (atom []))
(swap! bass-pulse
       (fn [_]
         {
          1 [grit-bass [
                   ;(midi->hz (choose (scale :C3 :minor))) :amp 0.5 :t 0.6
                                        ;:amp 0.5
                        :dur 1
                        :amp 1
                    ]]
          1.5 [grit-bass [
                     ;:amp 0.5
                     ;(midi->hz (choose (scale :C3 :minor))) :amp 0.5 :t 0.6
                      ]]
          ;1.75 []
          }
         )
       )




(comment
  (my-bass (choose (scale :C3 :major)) :amp 1)
  (bass (midi->hz (note :C3)))
  (stop)
  (s/play 1 @bass-line)
  (vintage-bass (choose (scale :C3 :major)) 50)
  (overpad (note :D4) 0.6)
  (s/add-p core/player bass-pulse :bass)
  (s/add-p core/player bass-line :bass-line)
  (s/rm-p core/player :bass)
  (s/wrap-p core/player :bass)
  (s/rm-p core/player :bass-line)
  (s/add-p melissa :bass)
  )
