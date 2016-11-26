(ns techno.melody
  (:use
   [overtone.algo.chance]
   [overtone.music.pitch]))

(defn perfect-unison [degree]
  "Identical notes. No interval jump."
  (+ 0 degree))

(defn up-step [degree]
  "One step up in the scale (Whole note or half, depending on degree)."
  (inc degree))

(defn down-step [degree]
  "One step down in the scale (Whole note or half, depending on degree)."
  (dec degree))

(defn up-leap [degree]
  "One leap up in the scale (More than 2 semitones)."
  (+ (rand-nth [2 3 4 5 6]) degree))

(defn down-leap [degree]
  "One leap down in the scale (More than 2 semitones)."
  (- (rand-nth [2 3 4 5 6]) degree))

(defn up-octave [degree]
  "One perfect octave up (Example: C3 to C4)."
  (+ 7 degree))

(defn down-octave [degree]
  "One perfect octave down (Example: C4 to C3)."
  (- 7 degree))

(defn weighted-random-interval-jumps [scale degree weights]
  "Generate an interval jump using weighted random selection,
  from the current note until another compatible note is reached.
  The chances of steps, leaps and octave transitions vary."
  (let [movements '(perfect-unison up-step down-step up-leap down-leap up-octave down-octave)
        current-move (weighted-choose movements weights)
        temp-degree ((resolve current-move) degree)]
    (if (and (> temp-degree 0) (>= (count scale) temp-degree))
      temp-degree
      (weighted-random-interval-jumps scale temp-degree weights))))

(defn conjunct-motion [scale degree]
  "Melodic motion where steps are more likely to occur than leaps."
  (weighted-random-interval-jumps scale degree '(0.06 0.35 0.35 0.08 0.08 0.04 0.04)))

(defn disjunct-motion [scale degree]
  "Melodic motion where leaps are more likely to occur than steps."
  (weighted-random-interval-jumps scale degree '(0.06 0.08 0.08 0.30 0.30 0.09 0.09)))

(defn construct-intervals [construct-melody scale note-count
                           & {:keys [running-interval degree]
                              :or {running-interval '()
                                   degree 1}}]
  "Build an interval-representation of the melody
  by traversing through the scale in steps/leaps/octave jumps."
  (if (<= note-count 0)
    running-interval
    (let [degree (construct-melody scale degree)
          running-interval (concat running-interval (list degree))
          note-count (dec note-count)]
      (construct-intervals construct-melody
                           scale
                           note-count
                           :degree degree
                           :running-interval running-interval))))

(defn generate-intervals [construct-melody scale note-count & [degree]]
  "Start from the first note, construct intervals,
  end at the first or last note."
  (let [degree (if degree degree (- note-count 2))
        mid-intervals (construct-intervals construct-melody scale degree)
        scale-count (count scale)]
    (concat '(1)
            mid-intervals
            (list (rand-nth `(1 ~scale-count))))))

(defn intervals->notes [intervals scale]
  "Convert intervals to notes in a scale."
  (map #(nth scale (dec %)) intervals))

(defn gen-phrase [scale note-count & {:keys [degree step leap fit rests max-block]
                                      :or {leap true step false fit 1.75
                                           max-block 2
                                           rests [:0 :1 :2]
                                           degree 1}}]
  (let [notes (vec (map
                    find-note-name
                    (intervals->notes
                     (generate-intervals
                      (if step conjunct-motion disjunct-motion) scale note-count)
                     scale)))
        notes (loop [phrase [] left notes]
                (let [num (rand-int (inc max-block))
                      num (if (> num 0) num 1)
;                      num (if (> (dec num) (count left)) 1 num)
                      action (vec (take num left))
                      left (if (<= num (count left))
                             (subvec left num)
                             (vec (rest left)))
                      r  (rand-nth rests)
                      phrase (conj phrase action r)]
                  (if (> (count left) 0)
                    (recur phrase
                             left)
                      phrase))
                )]
    notes
    )
  )
