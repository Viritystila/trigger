# trigger

A Clojure library designed to create control patterns to Overtone synths. The library is still in it early stages.

## Usage

#### Define synth with control-bus inputs
(defsynth tstsin [in-trg 0 in-trg-val 0 in-attack 0 in-attack-val 0 f 200 out-bus 0] (let [trg (in:kr in-trg)
                                                                                           val (in:kr in-trg-val)
                                                                                           env (env-gen (perc (in:kr in-attack-val) 0.01 1 0) :gate trg)
                                                                                           src (* env (sin-osc (* f val)))]
                                                                                                                                                                                         (out out-bus src)))

Here, in-trg and in-attack are specified as control values in trigger. The software also connects automatically to in-trg-val and in-attack-val. No trigger is created if no corresponding arguemnt is found in the synthdef.

##### start the pattern

(-> {:pn "tstsin2" :sn tstsin :in-trg ["[1 1 1 1]"] :in-attack ["[0.05]"] } trg)

:pn = Pattern name. The pattern and the trigger synths are stored under the name
:sn = Name of the synth to be used

Pattern structure
["[p1]"  "[p2]"  "[p3]" .. . ]

example:
["[1]"] plays one whole note
["[1 1 1 1]"] plays four quarter notes per second.
["[1 1 1 1]" "[2]"] plays a pattern with four quarter note and then a pattern with a one whole note with an value of 2.


####
As in TidalCycles the duration of each pattern  [p] is one second, hence adding a pattern adds one second to the full cycle duration.  The value in the pattern structure is passed in the "*-val" control-bus.  


### TODO:
  - Monitor bus selection for visualizations
  - Managing the output bus
  - Output effects
## License
The MIT License

Copyright © 2019 Mikael Reponen

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
