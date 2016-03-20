(ns techno.drums
  (:use [overtone.core]
        [overtone.inst.drum]
        )
  )


(dance-kick :decay 10)


(def beat-sequence
  [
   [open-hat :decay 0.5]
   [kick]
   [closed-hat]
   [dance-kick]
   ]
  )




(kick)

(dance-kick 100)


                                        ;Patterns


;Playing functions
