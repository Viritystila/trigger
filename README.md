# trigger

A Clojure library designed to create control patterns to Overtone synths. The library is still in it early stages.

## Usage

#### Define synth with control-bus inputs
``` 

(defsynth ping
  [in-trg 0
   in-trg-val 0
   in-note 72
   in-note-val 72
   in-attack 0.02
   in-attack-val 0.02
   in-decay 0.3
   in-decay-val 0.3
   out-bus 0]
  (let [note   (in:kr in-note-val)
        attack (in:kr in-attack-val)
        decay  (in:kr in-decay-val)
        snd    (sin-osc (midicps note))
        env    (env-gen (perc attack decay) :gate (in:kr in-trg))]
    (out out-bus (pan2 (* 0.8 env snd)))))
``` 

Here, in-trg and in-attack are specified as control values in trigger. The software also connects automatically to in-trg-val and in-attack-val. No trigger is created if no corresponding arguemnt is found in the synthdef.

##### start the pattern

(trg "png" ping :in-trg [1] :in-note [72])


"png" = Pattern name. The pattern and the trigger synths are stored under the name
ping = Name of the synth to be used

Pattern structure
:in-trg [p1] [p2] [p3] ... [pn]
The patterns accept clojure functions as input, for example (repeat 4 1). If the function returns a seq, the seq is joined into the containing pattern. For example:
[1 1 (repeat 2 1)] = [1 1 1 1]


Ping example:
(trg "png" ping :in-trg [1] :in-note [72]) ,   plays one whole note with note 72.

(trg "png" ping :in-trg [1 1 1 1] :in-note [77]) plays four quarter notes per second with note 77..

In,
(trg "png" ping :in-trg [1 1] [(repeat 4 1)] [r] [1 [r 1]] :in-note [ 72 75] [72] [72] [70 [72 72]])


the in:trg and in:note loop 4 patterns, each 1 second long. Here, 'r' denotes a rest. 

The duration of each pattern  [p] is one second, hence adding a pattern adds one second to the full cycle duration.  The value in the pattern structure is passed in the "*-val" control-bus.  


### TODO:
  - Managing the output bus
  - Output effects
## License
The MIT License

Copyright © 2019 Mikael Reponen

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
