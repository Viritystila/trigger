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
;(-> {:pn "flute" :sn simple-flute :in-trg ["[1]" "[1 1 1 1]"] :in-freq ["[300 200 100 300 200 100 300 200 100]" "[100]" "[200 100]" "[50 150 200 25]"] :in-amp ["[10]"] :in-attack ["[0.04]"] :in-decay ["[0.5]"] :in-sustain ["[1]"] :in-release ["[0.8]"]} trg )


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
;(-> {:pn "cs80lead" :sn cs80lead :in-trg ["[1]"] :in-freq ["[30 40]"] :in-amp ["[0.4]"] :in-attack ["[0.75]"] :in-vibdepth ["[1.0001]"] } trg )


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
;(-> {:pn "supersaw " :sn supersaw :in-freq ["[51]" "[55 60 65 55]" "[50]"] :in-amp ["[0.4]" "[0.4 0.45 0.5 0.55 0.6 0.55 0.5 0.45 0.4]" "[0.4]"] } trg )


(defsynth ticker
  [in-trg 880 in-trg-val 880 out-bus 0]
  (* (env-gen (perc 0.001 0.01) :gate (in:kr in-trg))
     (out out-bus (pan2 (sin-osc (in:kr in-trg-val))))))
;(-> {:pn "ticker" :sn ticker :in-trg ["[51]" "[55 60 65 55]" "[50]"] } trg )

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
;(-> {:pn "ping" :sn ping :in-trg ["[51 52 54 55 60 70 80 90]" "[1 1 1 1]"] :in-note ["[50]"] :in-decay ["[0.2]"]} trg)


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
;(-> {:pn "tb303" :sn tb303 :in-trg ["[1 1 1 1]"] :in-note ["[40]"] :in-attack ["[0.001]"] :in-decay ["[0.008]"] :in-sustain ["[0.99]"] :in-release ["[1.05]"] :in-amp ["[0.35]"] :in-cutoff ["[402]"] :in-wave ["[2]"] } trg )



(defsynth mooger
  "Choose 0, 1, or 2 for saw, sin, or pulse"
  [in-trg 0
   in-trg-val 0
   in-note 60
   in-note-val 60
   in-amp 0.3
   in-amp-val 0.3
   in-osc1 1
   in-osc1-val 1
   in-osc2 1
   in-osc2-val 1
   in-cutoff 500
   in-cutoff-val 500
   in-attack 0.0001
   in-attack-val 0.0001
   in-decay 0.3
   in-decay-val 0.3
   in-sustain 0.99
   in-sustain-val 0.99
   in-release 0.0001
   in-release-val 0.0001
   in-fattack 0.0001
   in-fattack-val 0.0001
   in-fdecay 0.3
   in-fdecay-val 0.3
   in-fsustain 0.999
   in-fsustain-val 0.999
   in-frelease 0.0001
   in-frelease-val 0.0001
   in-osc1-level 0.5
   in-osc1-level-val 0.5
   in-osc2-level 0.5
   in-osc2-level-val 0.5
   out-bus 0]
  (let [gate           (in:kr in-trg)
        gate-val       (in:kr in-trg-val)
        note           (in:kr in-note-val)
        amp            (in:kr in-amp-val)
        osc1           (in:kr in-osc1-val)
        osc2           (in:kr in-osc2-val)
        cutoff         (in:kr in-cutoff-val)
        attack         (in:kr in-attack-val)
        decay          (in:kr in-decay-val)
        sustain        (in:kr in-sustain-val)
        release        (in:kr in-release-val)
        fattack        (in:kr in-fattack-val)
        fdecay         (in:kr in-fdecay-val)
        fsustain       (in:kr in-fsustain-val)
        frelease       (in:kr in-frelease-val)
        osc1-level     (in:kr in-osc1-level-val)
        osc2-level     (in:kr in-osc2-level-val)
        freq       (midicps note)
        osc-bank-1 [(saw freq) (sin-osc freq) (pulse freq)]
        osc-bank-2 [(saw freq) (sin-osc freq) (pulse freq)]
        amp-env    (env-gen (adsr attack decay sustain release) :gate gate-val)
        f-env      (env-gen (adsr fattack fdecay fsustain frelease) :gate gate-val)
        s1         (* osc1-level (select osc1 osc-bank-1))
        s2         (* osc2-level (select osc2 osc-bank-2))
        filt       (moog-ff (+ s1 s2) (* cutoff f-env) 3)]
    (out out-bus (pan2 (* amp amp-env filt)))))
;(-> {:pn "mooger" :sn mooger :in-trg ["[1 1 1 [1 1 1 1]]" "[1 0 1 1 0 1 1 [1 1 1 1]]"] :in-note ["[40 45]"] :in-attack ["[0.0022]"] :in-decay ["[1.091]"] :in-sustain ["[0.0099]"] :in-release ["[1.0001]"] :in-amp ["[0.9]"] :in-cutoff ["[500 600]"] :in-osc1 ["[1 2]"] :in-osc2 ["[1 2]"] :in-osc1-level ["[0.95]"] :in-osc2-level ["[0.5]"] :in-fattack ["[0.022]"] :in-fdecay ["[0.091]"] :in-fsustain ["[0.099]"] :in-frelease ["[0.9 0.99]"] } trg )
