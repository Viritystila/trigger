                                                             

```                                                             
    ▒            ░██                                         
    █                                                        
  █████ ████████░███   ███ ██░█  ██ ░██░░ ▒██ ██  ███▒████   
   ██    ░██      ██   ██  ▓██  ███  ██░ ███▒▒▒██  ███       
   ██░ █ ░██      ██   ▓█▓░░     ██▒░    ▓██    █  ███       
   ▓███ ██████  ▒████░ ███████░ ████████   █████  █████      
                      ██░   ░█░ ██    ██                     

```                                                                        

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

The functions below assume the following

```
(ns test
  (:use [trigger.trigger]
        [trigger.synths]
        [trigger.algo]
        [trigger.speech]
        [trigger.samples]
        [trigger.trg_fx] 
        [overtone.core]) )

```


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

Trigger includes few scpecial strigns to be sued with the patterns. For example ["n c1"] inputs the midi note of c1 into the patterns and ["f c1"] inputs the frequency of c1. ["b audio-buffer"] input the bufer id of an audio buffer into the pattern to be used with the ``` smp ``` sampler synth. For example, if the SuperDirst samples have been installed via SuperColliderm they can be loaded using function

```
(load-all-SuperDirt-samples)
```

After this, the samples can be used 

```
(trg :smp smp
     :in-trg [1]
     :in-step [2]
      :in-buf ["b hc4" "b hc3" "b hc4" "b hc3"] )
```

Alternatively, a trigger pattern can be coped to another pattern, for example as.

```
(trg :smp smp
     :in-trg ["b hc4" "b hc3" "b hc4" "b hc3"]
     :in-step [2]
      :in-buf ":in-trg" )
```

##### Text-to-Speech. 

Trigger utilizes MaryTTS for generating spoken words from text. For example

```
(add-sample "test" (string-to-buffer "This is a test))

(trg :smp smp
     :in-trg ["b test"]
     :in-step [2]
      :in-buf ":in-trg" )

```


##### Simple modular synthesis
``` (trg! ([pn spn sn & input]) ``` function can be used to add a add and control subsynths, for example one providing echo, to the main synth. These synths need to be defined for example as

```(defsynth trg-fx-rlpf
  [bus-in 0
   in-cutoff 20000
   in-res 0.6
   in-cutoff-val 20000
   in-res-val 0.6
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [src (in bus-in)
        cutoff (in:kr in-cutoff-val)
        res     (in:kr in-res-val)
        snd  (rlpf src cutoff res)
        snd (select (in:kr in-out-select-val) [snd src])]
    (replace-out out-bus (pan2 snd))))
```

##### Algorithmic patterns

```(alg [pattern trigger buf-id function & args])``` function can be use to write to a buffer on a event from the trigger. 

###### Markov chain beat example

```
;Markov chain example
(def note-fobm {
                [:A2]  { :A2 0.1  :C#2 0.06  :E2 0.3 }
                [:C#2] { :A2 0.925 :C#2 0.05 :E2 0.07 }
                [:E2] { :A2 0.7  :C#2 0.03  :E2 0.9 }})



(def beat-fobm {
           [0.125] { 0.125 0.91  0.25 0.06  0.5 0.9   0.0625 0.9 }
           [0.25] {  0.125 0.92  0.25 0.05  0.5 0.9   0.0625 0.9 }
           [0.5] {   0.125 0.7   0.25 0.9   0.5 0.09  0.0625 0.9}
           [0.0625] {  0.125 0.09  0.25 0.04  0.5 0.01  0.0625 0.9}
           })

(defn example_markov [t-id alg-key pat-vec pat-val-vec buf-id alg-config & args]
  (on-trigger t-id
              (fn [val] (try (let [fobm_args (first (first args))
                                  buf  (nth pat-vec buf-id)
                                  rnmd (+ 1 (rand-int 7))
                                  dur  (/ 1 rnmd)]  (vec (take 1 (markov-chains.core/generate @fobm_args)))
                              (buffer-write! buf 1 dur)
                              )
                            (catch Exception e (do (println "Excpetion " e)
                                                   (swap! alg-config dissoc alg-key)
                                                   (remove-event-handler alg-key)))))
              alg-key))



(defn example_markov2 [t-id alg-key pat-vec pat-val-vec buf-id alg-config & args]
  (on-trigger t-id
              (fn [val] (try (let [fobm_args (first (first args))
                                  fobm_args2 (last (first args))
                                  buf  (nth pat-vec buf-id)
                                  buf2 (nth pat-val-vec buf-id)
                                  rnmd (+ 1 (rand-int 7))
                                  dur  (/ 1 rnmd)
                                  dur  (vec (take 1 (markov-chains.core/generate @fobm_args)))
                                  note_m (note (first (take 1 (markov-chains.core/generate @fobm_args2))))
                                  freq [(midi->hz note_m)]]
                              (buffer-write! buf 1 dur)
                              (buffer-write! buf2 1 freq)
                              )
                            (catch Exception e (do (println "Excpetion " e)
                                                   (swap! alg-config dissoc alg-key)
                                                 (remove-event-handler alg-key)))))
              alg-key))


```


For ```(trg :png ping :in-trg [1] :in-note [77]) ``` the alg usgae is the following. Get the buffer ids of the patterns via for example
```
(def ba (atom beat-fobm))
(alg :png :in-trg 0 example_markov ba)
```

The alg can be then removed with ```(rm-alg :png :in-trg 0) ```


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

(lss) - list running patterns

(set-mixer-out-channel [pattern-name channel]) Set the mixer out to channel, default channel 0

(pat [pattern-name trig-name]) Pause a trigger

(stt [pattern-name trig-name]) Start a trigger

(list-sub-synths [pattern-name]) List sub synths created with trg! for a pattern


```



The duration of each pattern  [p] is one second, hence adding a pattern adds one second to the full cycle duration. The duration of a single pattern can be adjusted with (set-pattern-duration) function.  The value in the pattern input is passed in the "*-val" control-bus.

Each pattern is controlled by its own trigger-generator synth which sends two trigger /tr signal on each beat. One of these contains the time untole the next trigger, and the second the value of the particular beat. The id's for these triggers can be retrieved with (get-trigger-id) and (get-trigger-val-id) functions. For example:

```
(on-trigger (get-trigger-val-id :png :in-trg) (fn [val] (println val)) ::ping-trig )
```


## License
The MIT License

Copyright © 2019 Mikael Reponen

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
