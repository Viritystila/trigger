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

Copyright © 2019 Mikael Reponen

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
