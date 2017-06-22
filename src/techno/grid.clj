(ns techno.grid
  (:use [techno.samples :only [drum-kits]]
        [overtone.sc.trig]
        [overtone.sc.node]
        [overtone.music.pitch]
        [overtone.libs.event :only [on-event event]])
  (:require [techno.sequencer :as s]
            [quil.core :as q]
            [quil.applet :as ap]
            )
  (:import [java.awt Color]
           [javax.swing JComboBox JTextArea GroupLayout JFrame JTextField JButton]
;           [controlP5 ControlP5]
           (java.awt Dimension Font Color)
           (java.awt.event ActionListener ActionEvent)))
