# Techno

A Sequencer/DSL for composing and improvising music using Clojure and Supercollider


## Prerequisites

Java https://jdk.java.net/

Leiningen https://leiningen.org/

Supercollider https://supercollider.github.io/download

## Installation

In a command line shell

> cd [project-directory]

> lein install

## Usage

These tools are meant to be run in a clojure repl. The DSL described below is essentially a clojure vector.

### Supercollider

After Downloading and installing Supercollider, run supercollider and in the editor window run following commands


s.options.maxLogins = 2;

s.options.memSize_(65536 * 4);

s.boot;

### Command Line

After making sure supercollider is up and running.

> cd [project-directory]

> lein repl

#### Load core functions 

> (load-file "src/techno/core.clj")

> (ns techno.core)

#### Connect to supercollider

> (connect-sc)

#### Start a sequencer instance with 60 beats per minute

> (play-seq 60)

#### Add a pattern to start making music

> (add-pattern (phrase-p piano
    [[:d4 :f4 :c5] :| [:g4 :b5 :a4] :| [:c4 :e4 :g4]]
    1/4) :melody)

#### Remove a pattern

> (remove-pattern :melody)

#### Stop sequencer

> (pause-seq)

## Rationale

There are several excellent livecoding environments for music e.g. Supercollider, Max/MSP, TidalCycles, SonicPi . However I was unsatisfied with the available functionality to sequence conventional step driven music. Clojure being a lisp is extremely expressive and ideal for livecoding. Overtone provides a clojure based client for supercollider however it still lacks a robust sequencer. This project attempts to build on these libraries and provide a sequencing solution that is easy to use but still expressive.





## License
The MIT License (MIT)

Copyright © 2021 <Jaideep Umraiya>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
