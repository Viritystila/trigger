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


(defsynth cs80lead
  [in-trg 0
   in-trg-val 0
   in-freq 880
   in-freq-val 880
   in-amp 0.5
   in-amp-val 0.5
   in-attack 0.75
   in-attack-val 0.75
   in-decay 0.5
   in-decay-val 0.5
   in-sustain 0.8
   in-sustain-val 0.8
   in-release 1
   in-release-val 1
   in-fattack 0.75
   in-fattack-val 0.75
   in-fdecay 0.5
   in-fdecay-val 0.5
   in-fsustain 0.8
   in-fsustain-val 0.8
   in-frelease 1
   in-frelease-val 1
   in-cutoff 200
   in-cutoff-val 200
   in-dtune 0.002
   in-dtune-val 0.002
   in-vibrate 4
   in-vibrate-val 4
   in-vibdepth 0.015
   in-vibdepth-val 0.015
   in-fre-lag 0.1
   in-fre-lag-val 0.1
   out-bus 0]
  (let [freq     (in:kr in-freq-val)
        freq-lag (in:kr in-fre-lag-val)
        freq     (lag freq freq-lag)
        amp      (in:kr in-amp-val)
        gate     (in:kr in-trg)
        cuttoff  (in:kr in-cutoff-val)
        att      (in:kr in-attack-val)
        decay    (in:kr in-decay-val)
        sus      (in:kr in-sustain-val)
        rel      (in:kr in-release-val)

        fatt     (in:kr in-fattack-val)
        fdecay   (in:kr in-fdecay-val)
        fsus     (in:kr in-fsustain-val)
        frel     (in:kr in-frelease-val)

        dtune    (in:kr in-dtune-val)
        vibrate  (in:kr in-vibrate-val)
        vibdepth (in:kr in-vibdepth-val)

        env     (env-gen (adsr att decay sus rel) :gate gate)
        fenv    (env-gen (adsr fatt fdecay fsus frel 2) :gate gate)

        vib     (+ 1 (lin-lin:kr (sin-osc:kr vibrate) -1 1 (- vibdepth) vibdepth))

        freq    (* freq vib)
        sig     (mix (* env amp (saw [freq (* freq (+ dtune 1))])))]
(out out-bus (pan2 sig))))


(defsynth supersaw [in-freq 440 in-freq-val 440 in-amp 1 in-amp-val 1 out-bus 0]
  (let [freq   (in:kr in-freq-val)
        amp    (in:kr in-amp-val)
        input  (lf-saw freq)
        shift1 (lf-saw 4)
        shift2 (lf-saw 7)
        shift3 (lf-saw 5)
        shift4 (lf-saw 2)
        comp1  (> input shift1)
        comp2  (> input shift2)
        comp3  (> input shift3)
        comp4  (> input shift4)
        output (+ (- input comp1) (- input comp2) (- input comp3) (- input comp4))
        output (- output input)
        output (leak-dc:ar (* output 0.25))]
    (out out-bus (pan2 (* amp output)))))

(defsynth ticker
  [in-trg 880 in-trg-val 880 out-bus 0]
  (* (env-gen (perc 0.001 0.01) :gate (in:kr in-trg))
     (out out-bus (pan2 (sin-osc (in:kr in-trg-val))))))


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


(defsynth tb303
  [in-trg 0
   in-trg-val 0
   in-wave 1
   in-wave-val 1
   in-r 0.8
   in-r-val 0.8
   in-note 60
   in-note-val 60
   in-attack 0.01
   in-attack-val 0.01
   in-decay 0.1
   in-decay-val 0.1
   in-sustain 0.6
   in-sustain-val 0.6
   in-release 0.01
   in-release-val 0.01
   in-cutoff 100
   in-cutoff-val 100
   in-env-amount 0.01
   in-env-amount-val 0.01
   in-amp 0.5
   in-amp-val 0.5
   out-bus 0]
  (let [note       (in:kr in-note-val)
        wave       (in:kr in-wave-val)
        r          (in:kr in-r-val)
        attack     (in:kr in-attack-val)
        decay      (in:kr in-decay-val)
        sustain    (in:kr in-sustain-val)
        release    (in:kr in-release-val)
        cutoff     (in:kr in-cutoff-val)
        env-amount (in:kr in-env-amount-val)
        amp        (in:kr in-amp-val)
        freq       (midicps note)
        freqs      [freq (* 1.01 freq)]
        vol-env    (env-gen (adsr attack decay sustain release)
                            (line:kr 1 0 (+ attack decay release))
                            :gate (in:kr in-trg))
        fil-env    (env-gen (perc))
        fil-cutoff (+ cutoff (* env-amount fil-env))
        waves      (* vol-env
                      [(saw freqs)
                       (pulse freqs 0.5)
                       (lf-tri freqs)])
        selector   (select wave waves)
        filt       (rlpf selector fil-cutoff r)]
    (out out-bus (pan2 (* amp filt))))
  )
