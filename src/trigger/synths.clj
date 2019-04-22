(ns #^{:author "Mikael Reponen"}
  trigger.synths
  (:use [overtone.core]))

                                        ;Synths
                                        ;Some of the definitions adapted from
                                        ;https://github.com/overtone/overtone/blob/master/src/overtone/inst/synth.clj
(defsynth testsin [in-trg 0 in-trg-val 0 in-attack 0 in-attack-val 0 f 200 out-bus 0] (let [trg (in:kr in-trg)
                                                                                           val (in:kr in-trg-val)
                                                                                           env (env-gen (perc (in:kr in-attack-val) 0.01 1 0) :gate trg)
                                                                                           src (* env (sin-osc (* f val)))]
                                                                                       (out out-bus src)))


(defsynth simple-flute [in-trg 0
                        in-trg-val 0
                        in-freq 880
                        in-freq-val 880
                        in-amp 1
                        in-amp-val 0.5
                        in-attack 0.4
                        in-attack-val 0.4
                        in-decay 0.5
                        in-decay-val 0.5
                        in-sustain 0.8
                        in-sustain-val 0.8
                        in-release 1
                        in-release-val 1
                       out-bus 0]
  (let [gate    (in:kr in-trg)
        freq    (in:kr in-freq-val)
        amp     (in:kr in-amp-val)
        attack  (in:kr in-attack-val)
        decay   (in:kr in-decay-val)
        sustain (in:kr in-sustain-val)
        release (in:kr in-release-val)
        env  (env-gen (adsr attack decay sustain release) :gate gate)
        mod1 (lin-lin:kr (sin-osc:kr 6) -1 1 (* freq 0.99) (* freq 1.01))
        mod2 (lin-lin:kr (lf-noise2:kr 1) -1 1 0.2 1)
        mod3 (lin-lin:kr (sin-osc:kr (ranged-rand 4 6)) -1 1 0.5 1)
        sig (distort (* env (sin-osc [freq mod1])))
        sig (* amp sig mod2 mod3)]
    (out out-bus (pan2 sig))))
