# trigger

A program designed to create control patterns for Overtone synths.

## Usage

#### Define synth with control-bus inputs
Trigger includex some built in synths already, with more added form time to time (see synths.clj under src/). An example of the basic synth arg structure is:

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

Here, in-trg and in-attack are specified as control values in trigger. The software also connects automatically to in-trg-val and in-attack-val. No trigger is created if no corresponding argument is found in the synthdef.

##### start the pattern
```
(trg :png ping :in-trg [1] :in-note [72])
```

"png" = Pattern name. The pattern and the trigger synths are stored under the name
ping = Name of the synth to be used

Pattern structure
```
:in-trg [p1] [p2] [p3] ... [pn]
```
The patterns accept clojure functions as input, for example (repeat 4 1). If the function returns a seq, the seq is joined into the containing pattern. For example:
```
[1 1 (repeat 2 1)] = [1 1 1 1]
```

Ping example:
```
(trg :png ping :in-trg [1] :in-note [72]) 
```
,   plays one whole note with note 72.
```
(trg :png ping :in-trg [1 1 1 1] :in-note [77]) 
```
plays four quarter notes per second with note 77..

In,

```
(trg :png ping :in-trg [1 1] [(repeat 4 1)] [r] [1 [r 1]] :in-note [ 72 75] [72] [72] [70 [72 72]])
```
the in:trg and in:note loop 4 patterns, each 1 second long. Here, 'r' denotes a rest. 

##### Other functions:

```
(stp [pattern-name]) - stop the pattern, takes the pattern-name as a keyword

(set-out-bus [pattern-name] ) - sets the synth output to the default out-bus (0)

(set-secondary-out-bus  [pattern-name]) - sets the output to a secondary audio-bus

(get-out-bus [pattern-name]) - get the default audio bus

(get-secondary-out-bus [pattern-name]) - get the secondary audio bus

(get-ctrl-bus [pattern-name]) - get the control out bus

(get-trigger-bus [pattern-name trig-name]) - get the trigger bus

(get-trigger-val-bus [pattern-name trig-name]) - get the trigger value bus

(get-trigger-id [pattern-name trig-name]) - get the trigger id

(get-trigger-val-id [pattern-name trig-name]) - get the trigger value id
```



The duration of each pattern  [p] is one second, hence adding a pattern adds one second to the full cycle duration. The duration of a single pattern can be adjusted with (set-pattern-duration) function.  The value in the pattern input is passed in the "*-val" control-bus.

Each pattern is controlled by its own trigger-generator synth which sends two trigger /tr signal on each beat. One of these contains the time untole the next trigger, and the second the value of the particular beat. The id's for these triggers can be retrieved with (get-trigger-id) and (get-trigger-val-id) functions. For example:

```
(on-trigger (get-trigger-val-id :png :in-trg) (fn [val] (println val)) ::ping-trig )
```



### TODO:
  - Syncronizing with TidaCycles
  - Output effects
## License
The MIT License

Copyright © 2019 Mikael Reponen

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
