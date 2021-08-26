(ns techno.helper
  "Various helper functions"
  )

(defn get-pos [beat div & [size s-div]]
  "Given a offset in the sequence and a subdivision of pattern and sequencer, returns the corresponding bar and beat"
  (let [step (if s-div (/ s-div div) div)
        beat (cond (= size 1) 1
                   (and size (> beat size))
                   (if (= (mod beat size) 0)
                     (min size div)
                     (mod beat size))
                   true beat)
        ret-beat (if s-div (or (= 1 beat) (= 0 (mod (dec beat) step))) true)
        bar (cond s-div (inc (int (/ (dec beat) s-div)))
              (= 0 (mod beat div)) (/ beat div)
              true (inc (int (/ beat div))))
        n (if s-div
            (inc (int (/ (dec (mod beat s-div)) step)))
            (mod beat div))
        n (if (= 0 n) div n)]
    (if ret-beat
      [(int bar) (int n)]
      [0 0])
    ))

(defn get-beat [bar note div]
  "Reverse of get-pos, converts [bar note] into a single number in the sequence"
  (+ (* (dec bar) div) note)
  )

(defn p-size [p & [s-div ret-pos]]
  "Returns the size of a pattern"
  (let [b (if (contains? p :p-size) (first (:p-size p))
              (apply max (filter number? (keys p))))
        note (if (contains? p :p-size)
               (second (:p-size p))
               (if (not (empty? (get p b)))
                 (apply max (keys (get p b)))
                 1))
        step (if (not (nil? s-div)) (/ s-div (get p :div)) 1)
        div (if s-div s-div (:div p))
        s (* (get p :div) (dec b) step)
        s (+ s (* step note))]
    (if ret-pos
      (get-pos s div)
        s)
    )
  )

(defn gcd
      [a b]
      (if (zero? b)
      a
      (recur b, (mod a b))))

(defn lcm [a b]
  "Lowest common multiple"
      (/ (* a b) (gcd a b)))
;; to calculate the lcm for a variable number of arguments
(defn lcmv [& v] (reduce lcm v))

(defn calc-period [bpm div]
  "Given a bpm, division and size of a sequencer, calculate period in ms"
  (/ 60000000 bpm div))
