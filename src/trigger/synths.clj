(ns #^{:author "Mikael Reponen"}
  trigger.synths
  (:use [overtone.core]))

                                        ;Synths
                                        ;Some of the synth  definitions adapted from
                                        ;https://github.com/overtone/overtone/blob/master/src/overtone/inst/synth.clj



(defsynth testsin [in-trg 0 in-trg-val 0 in-attack 0.0001 in-attack-val 0.0001 f 200 out-bus 0 ctrl-out 0] (let [trg (in:kr in-trg)
                                                                                                                 val (in:kr in-trg-val)
                                                                                                                 env (env-gen (perc (in:kr in-attack-val) 0.01 0.5 0.1) :gate trg)
                                                                                                                 src (* env (sin-osc (* f val)))]
                                                                                                             (out out-bus (pan2 src))))


(defsynth trg-sampler [in-trg 0
                       in-trg-val 0
                       in-buf 0
                       in-buf-val 0
                       in-rate 24000
                       in-rate-val 24000
                       in-start-pos 0
                       in-start-pos-val 0
                       in-step 2
                       in-step-val 2
                       in-loop 0
                       in-loop-val 0
                       in-amp 1
                       in-amp-val 1
                       out-bus 0
                       ctrl-out 0
                       bus-in 0]
  (let [trg       (in:kr in-trg)
        buf-trg   (in:kr in-buf)
        buf-no    (in:kr in-buf-val)
        amp       (in:kr in-amp-val)
        rate      (in:kr in-rate-val)
        start-pos (in:kr in-start-pos-val)
        step      (in:kr in-step-val)
        loop      (in:kr in-loop-val)
        dur       (/ 1.0 24000)
        td        (t-duty:ar dur 0 1 )
        buffer-value  (demand:ar td trg (dbufrd buf-no (dseries start-pos step  INF) loop))
        ]
    (out out-bus (pan2 (* amp buffer-value)))))


(defsynth smp [in-trg 0
               in-trg-val 0
               in-buf 0
               in-buf-val 0
               in-rate 24000
               in-rate-val 24000
               in-start-pos 0
               in-start-pos-val 0
               in-step 2
               in-step-val 2
               in-loop 0
               in-loop-val 0
               in-amp 1
               in-amp-val 1
               out-bus 0
               ctrl-out 0
               bus-in 0]
  (let [trg       (in:kr in-trg)
        buf-trg   (in:kr in-buf)
        buf-no    (in:kr in-buf-val)
        amp       (in:kr in-amp-val)
        rate      (in:kr in-rate-val)
        start-pos (in:kr in-start-pos-val)
        step      (in:kr in-step-val)
        loop      (in:kr in-loop-val)
        dur       (/ 1.0 (/ 44100 2))
        td        (t-duty:ar dur 0 1 )
        out-pos   (demand:ar td trg (dseries start-pos step INF))
        buffer-value  (demand:ar td trg (dbufrd buf-no (dseries start-pos step  INF) loop))
        ]
    (out:kr ctrl-out (a2k out-pos))
    (out out-bus (pan2 (* amp buffer-value)))))


(defsynth smp2 [in-trg1 0
                in-trg1-val 0
                in-buf1 0
                in-buf1-val 0
                in-rate1 24000
                in-rate1-val 24000
                in-start-pos1 0
                in-start-pos1-val 0
                in-step1 2
                in-step1-val 2
                in-loop1 0
                in-loop1-val 0
                in-amp1 1
                in-amp1-val 1
                in-trg2 0
                in-trg2-val 0
                in-buf2 0
                in-buf2-val 0
                in-rate2 24000
                in-rate2-val 24000
                in-start-pos2 0
                in-start-pos2-val 0
                in-step2 2
                in-step2-val 2
                in-loop2 0
                in-loop2-val 0
                in-amp2 1
                in-amp2-val 1
                out-bus 0
                ctrl-out 0
                bus-in 0]
  (let [trg1       (in:kr in-trg1)
        buf-trg1   (in:kr in-buf1)
        buf-no1    (in:kr in-buf1-val)
        amp1       (in:kr in-amp1-val)
        rate1      (in:kr in-rate1-val)
        start-pos1 (in:kr in-start-pos1-val)
        step1      (in:kr in-step1-val)
        loop1      (in:kr in-loop1-val)
        dur1       (/ 1.0 24000)
        td1        (t-duty:ar dur1 0 1 )
        buffer-value1  (demand:ar td1 trg1 (dbufrd buf-no1 (dseries start-pos1 step1  INF) loop1))
        trg2       (in:kr in-trg2)
        buf-trg2   (in:kr in-buf2)
        buf-no2    (in:kr in-buf2-val)
        amp2       (in:kr in-amp2-val)
        rate2      (in:kr in-rate2-val)
        start-pos2 (in:kr in-start-pos2-val)
        step2      (in:kr in-step2-val)
        loop2      (in:kr in-loop2-val)
        dur2       (/ 1.0 24000)
        td2        (t-duty:ar dur2 0 1 )
        buffer-value2  (demand:ar td2 trg2 (dbufrd buf-no2 (dseries start-pos2 step2  INF) loop2))
        ]
    (out out-bus (pan2 (+ (* amp1 buffer-value1) (* amp2 buffer-value2) )))))


(defsynth simple-flute [in-trg 0
                        in-trg-val 0
                        in-freq 880
                        in-freq-val 880
                        in-amp 1
                        in-amp-val 0.5
                        in-attack 0.001
                        in-attack-val 0.001
                        in-decay 0.5
                        in-decay-val 0.5
                        in-sustain 0.8
                        in-sustain-val 0.8
                        in-release 1
                        in-release-val 1
                        in-gate-select 0
                        in-gate-select-val 0
                        in-ctrl-select 0
                        in-ctrl-select-val 0
                        out-bus 0
                        ctrl-out 0]
  (let [gate           (in:kr in-trg)
        gate-val       (in:kr in-trg-val)
        trg-gate       (trig gate gate-val)
        gate           (select:kr (in:kr in-gate-select-val)  [trg-gate gate])
        freq           (in:kr in-freq-val)
        amp            (in:kr in-amp-val)
        attack         (in:kr in-attack-val)
        decay          (in:kr in-decay-val)
        sustain        (in:kr in-sustain-val)
        release        (in:kr in-release-val)
        env            (env-gen (adsr attack decay sustain release) :gate gate)
        mod1           (lin-lin:kr (sin-osc:kr 6) -1 1 (* freq 0.99) (* freq 1.01))
        mod2           (lin-lin:kr (lf-noise2:kr 1) -1 1 0.2 1)
        mod3           (lin-lin:kr (sin-osc:kr (ranged-rand 4 6)) -1 1 0.5 1)
        sig            (distort (* env (sin-osc [freq mod1])))
        sig            (* amp sig mod2 mod3)
        ctrl-out-sel   (select:kr (in:kr in-ctrl-select-val) [(a2k sig) (a2k env)] )]
    (out:kr ctrl-out (a2k ctrl-out-sel))
    (out out-bus (pan2 sig))))

;(-> {:pn "flute" :sn simple-flute :in-trg ["[1]"]} trg)

;(-> {:pn "flute" :sn simple-flute :in-trg ["[1]" "[1 1 1 -1]"] :in-freq ["[300 200 100 300 200 100 300 200 100]" "[100]" "[200 100]" "[50 150 200 25]"] :in-amp ["[1]"] :in-attack ["[0.04]"] :in-decay ["[0.05]"] :in-sustain ["[0.5]"] :in-release ["[0.08]"] :in-gate-select ["[1]"]} trg )


(defsynth cs80lead
  [in-trg 0
   in-trg-val 0
   in-freq 60
   in-freq-val 60
   in-amp 0.5
   in-amp-val 0.5
   in-attack 0.001
   in-attack-val 0.001
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
   in-gate-select 0
   in-gate-select-val 0
   out-bus 0
   ctrl-out 0]
  (let [freq     (in:kr in-freq-val)
        freq-lag (in:kr in-fre-lag-val)
        freq     (lag freq freq-lag)
        amp      (in:kr in-amp-val)
        gate     (in:kr in-trg)
        gate-val (in:kr in-trg-val)
        trg-gate (trig gate gate-val)
        gate     (select:kr (in:kr in-gate-select-val)  [trg-gate gate])
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
        env      (env-gen (adsr att decay sus rel) :gate gate)
        fenv     (env-gen (adsr fatt fdecay fsus frel 2) :gate gate)
        vib      (+ 1 (lin-lin:kr (sin-osc:kr vibrate) -1 1 (- vibdepth) vibdepth))
        freq     (* freq vib)
        sig      (mix (* env amp (saw [freq (* freq (+ dtune 1))])))]
    (out out-bus (pan2 sig))))

;(-> {:pn "cs80lead" :sn cs80lead :in-trg ["[1]"]} trg )

;(-> {:pn "cs80lead" :sn cs80lead :in-trg ["[1 1 1 1]"] :in-freq ["[30 40]"] :in-amp ["[0.4]"] :in-attack ["[0.75]"] :in-decay ["[0.1]"] :in-sustain ["[0.5]"] :in-release ["[0.7]"] :in-vibdepth ["[0.001]"] :in-vibrate ["[1]"] :in-dtune ["[0.00000001]"] :in-freq-lag ["[10]"] :in-gate-select ["[1]"] } trg )




(defsynth supersaw [in-freq 440 in-freq-val 440 in-amp 0.5 in-amp-val 0.5 out-bus 0 ctrl-out 0]
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

; (-> {:pn "ss" :sn supersaw} trg)
;(-> {:pn "supersaw " :sn supersaw :in-freq ["[51]" "[55 60 65 55]" "[50]"] :in-amp ["[0.4]" "[0.4 0.45 0.5 0.55 0.6 0.55 0.5 0.45 0.4]" "[0.4]"] } trg )


(defsynth ticker
  [in-trg 880 in-trg-val 880 ctrl-out 0  out-bus 0]
  (let [env (env-gen (perc 0.001 0.01) :gate (in:kr in-trg))]
    (out out-bus (pan2 (* env (sin-osc (in:kr in-trg-val)))))))
;(-> {:pn "ticker" :sn ticker :in-trg ["[51]" "[55 60 65 55]" "[50]"] } trg )

(defsynth ping
  [in-trg 0
   in-trg-val 0
   in-amp 0.8
   in-amp-val 0.8
   in-freq 72
   in-freq-val 72
   in-attack 0.02
   in-attack-val 0.02
   in-decay 0.3
   in-decay-val 0.3
   ctrl-out 0
   out-bus 0]
  (let [note   (in:kr in-freq-val)
        amp    (in:kr in-amp-val)
        attack (in:kr in-attack-val)
        decay  (in:kr in-decay-val)
        snd    (sin-osc note)
        env    (env-gen (perc attack decay) :gate (in:kr in-trg))]
    (out out-bus (pan2 (* amp env snd)))))
;(-> {:pn "ping" :sn ping :in-trg ["[1]"]} trg)
;(-> {:pn "ping" :sn ping :in-trg ["[51 52 54 55 60 70 80 90]" "[1 1 1 1]"] :in-note ["[50]"] :in-decay ["[0.2]"]} trg)


(defsynth tb303
  [in-trg 0
   in-trg-val 0
   in-wave 1
   in-wave-val 1
   in-r 0.8
   in-r-val 0.8
   in-freq 60
   in-freq-val 60
   in-attack 0.01
   in-attack-val 0.01
   in-decay 0.1
   in-decay-val 0.1
   in-sustain 0.6
   in-sustain-val 0.6
   in-release 0.3
   in-release-val 0.3
   in-cutoff 2000
   in-cutoff-val 2000
   in-env-amount 0.01
   in-env-amount-val 0.01
   in-amp 0.5
   in-amp-val 0.5
   in-gate-select 0
   in-gate-select-val 0
   ctrl-out 0
   out-bus 0]
  (let [note       (in:kr in-freq-val)
        wave       (in:kr in-wave-val)
        r          (in:kr in-r-val)
        attack     (in:kr in-attack-val)
        decay      (in:kr in-decay-val)
        sustain    (in:kr in-sustain-val)
        release    (in:kr in-release-val)
        cutoff     (in:kr in-cutoff-val)
        env-amount (in:kr in-env-amount-val)
        amp        (in:kr in-amp-val)
        freq       note
        freqs      [freq (* 1.01 freq)]
        gate       (in:kr in-trg)
        gate-val   (in:kr in-trg-val)
        trg-gate   (trig gate gate-val)
        gate       (select:kr (in:kr in-gate-select-val)  [trg-gate gate])
        vol-env    (env-gen (adsr attack decay sustain release)
                            (line:kr 1 0 (+ attack decay release))
                            :gate gate)
        fil-env    (env-gen (perc) :gate gate)
        fil-cutoff (+ cutoff (* env-amount fil-env))
        waves      (* vol-env
                      [(saw freqs)
                       (pulse freqs 0.5)
                       (lf-tri freqs)])
        selector   (select wave waves)
        filt       (rlpf selector fil-cutoff r)]
    (out out-bus (pan2 (* amp filt))))
  )
;(-> {:pn "tb303" :sn tb303 :in-trg ["[1]"]} trg )
;(-> {:pn "tb303" :sn tb303 :in-trg ["[1 1 0 [1 1]]"] :in-note ["[40]"] :in-attack ["[0.001]"] :in-decay ["[0.8]"] :in-sustain ["[0.99]"] :in-release ["[1.05]"] :in-amp ["[0.35]"] :in-cutoff ["[402]"] :in-wave ["[2]" "[1]"] :in-gate-select ["[1]"] } trg )



(defsynth mooger
  "Choose 0, 1, or 2 for saw, sin, or pulse"
  [in-trg 0
   in-trg-val 0
   in-freq 60
   in-freq-val 60
   in-amp 1
   in-amp-val 1
   in-osc1 1
   in-osc1-val 1
   in-osc2 1
   in-osc2-val 1
   in-cutoff 500
   in-cutoff-val 500
   in-attack 0.0022
   in-attack-val 0.0022
   in-decay 0.95
   in-decay-val 0.95
   in-sustain 0.4
   in-sustain-val 0.4
   in-release 0.3
   in-release-val 0.3
   in-fattack 0.22
   in-fattack-val 0.22
   in-fdecay 0.9
   in-fdecay-val 0.9
   in-fsustain 0.999
   in-fsustain-val 0.999
   in-frelease 0.001
   in-frelease-val 0.001
   in-osc1-level 0.5
   in-osc1-level-val 0.5
   in-osc2-level 0.5
   in-osc2-level-val 0.5
   in-gate-select 0
   in-gate-select-val 0
   ctrl-out 0
   out-bus 0]
  (let [gate           (in:kr in-trg)
        gate-val       (in:kr in-trg-val)
        trg-gate       (trig gate gate-val)
        gate           (select:kr (in:kr in-gate-select-val)  [trg-gate gate])
        note           (in:kr in-freq-val)
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
        freq           note
        osc-bank-1     [(saw freq) (sin-osc freq) (pulse freq)]
        osc-bank-2     [(saw freq) (sin-osc freq) (pulse freq)]
        amp-env        (env-gen (adsr attack decay sustain release) :gate gate)
        f-env          (env-gen (adsr fattack fdecay fsustain frelease) :gate gate)
        s1             (* osc1-level (select osc1 osc-bank-1))
        s2             (* osc2-level (select osc2 osc-bank-2))
        filt           (moog-ff (+ s1 s2) (* cutoff f-env) 3)]
    (out out-bus (pan2 (* amp amp-env filt)))))

(defsynth snare [in-trg 0
                 in-trg-val 0
                 in-amp 1
                 in-amp-val 1
                 in-fraction 1
                 in-fraction-val 1
                 in-attack 0.1
                 in-attack-val 0.1
                 in-sustain 0.01
                 in-sustain-val 0.01
                 in-release 0.1
                 in-release-val 0.1
                 in-cutoff 2000
                 in-cutoff-val 2000
                 ctrl-out 0
                 out-bus 0]
  (let [pls      (in:kr in-trg)
        fraction (in:kr in-fraction-val)
        amp      (in:kr in-amp-val)
        attack   (in:kr in-attack-val)
        sustain  (in:kr in-sustain-val)
        release  (in:kr in-sustain-val)
        cutoff   (in:kr in-cutoff-val)
        adj  1
        env (env-gen (lin attack sustain (* adj release) (* 0.1 adj)) :gate pls)
        snare (* 3 (pink-noise) (apply + (* (decay env [attack release]) [1 release])))
        snare (+ snare (bpf (* 4 snare) cutoff))
        snare (clip2 snare 1)]
    (out out-bus (pan2 (*  amp snare env)))))

(defsynth kick [in-trg 0
                in-trg-val 0
                in-amp 0.02
                in-amp-val 0.02
                in-v1 0.1
                in-v1-val 0.01
                in-v2 0.01
                in-v2-val 0.01
                in-v3 0.01
                in-v3-val 0.01
                in-c1 -20
                in-c1-val 20
                in-c2 -8
                in-c2-val -8
                in-c3 -8
                in-c3-val -8
                in-d1 1
                in-d1-val 1
                in-d2 1
                in-d2-val 1
                in-d3 1
                in-d3-val 1
                in-f1 80
                in-f1-val 80
                in-f2 30
                in-f2-val 30
                in-f3 80
                in-f3-val 80
                in-clipv 0.3
                in-clip-val 0.3
                ctrl-out 0
                out-bus 0]
  (let [pls       (in:kr in-trg)
        amp       (in:kr in-amp-val)
        v1        (in:kr in-v1-val)
        d1        (in:kr in-d1-val)
        f1        (in:kr in-f1-val)
        c1        (in:kr in-c1-val)
        v2        (in:kr in-v2-val)
        d2        (in:kr in-d2-val)
        f2        (in:kr in-f2-val)
        c2        (in:kr in-c2-val)
        v3        (in:kr in-v3-val)
        d3        (in:kr in-d3-val)
        f3        (in:kr in-f3-val)
        c3        (in:kr in-c3-val)
        clipv     (in:kr in-clip-val)
        co-env    (perc v1 d1 f1 c1)
        a-env     (perc v2 d2 f2 c2)
        osc-env   (perc v3 d3 f3 c3)
        cutoff    (lpf (pink-noise) (+ (env-gen co-env :gate pls) (* 1 20)))
        sound     (lpf (sin-osc (+ 0 (env-gen osc-env :gate pls) 20)) (* 200 1))
        env       (env-gen a-env :gate pls)
        output    (*  amp (+ cutoff sound) env)
        output    (free-verb output 0.1 0.3 0.1)
        ]
    (out out-bus (pan2 output))))

(defsynth rise-fall-pad
  [in-trg 0
   in-trg-val 0
   in-freq 440
   in-freq-val 440
   in-t 4
   in-t-val 4
   in-amt 0.3
   in-amt-val 0.3
   in-amp 0.8
   in-amp-val 0.8
   in-gate-select 0
   in-gate-select-val 0
   ctrl-out 0
   out-bus 0]
  (let [freq       (in:kr in-freq-val)
        gate       (in:kr in-trg)
        gate-val   (in:kr in-trg-val)
        trg-gate   (trig gate gate-val)
        gate       (select:kr (in:kr in-gate-select-val)  [trg-gate gate])
        t          (in:kr in-t-val)
        amt        (in:kr in-amt-val)
        amp        (in:kr in-amp-val)
        f-env      (env-gen (perc t t)  :gate gate)
        src        (saw [freq (* freq 1.01)])
        signal     (rlpf (* 0.3 src)
                         (+ (* 0.6 freq) (* f-env 2 freq)) 0.2)
        k          (/ (* 2 amt) (- 1 amt))
        distort    (/ (* (+ 1 k) signal) (+ 1 (* k (abs signal))))
        gate       (pulse (* 2 (+ 1 (sin-osc:kr 0.05))))
        compressor (compander distort gate 0.01 1 0.5 0.01 0.01)
        dampener   (+ 1 (* 0.5 (sin-osc:kr 0.5)))
        reverb     (free-verb compressor 0.5 0.5 dampener)
        echo       (comb-n reverb 0.4 0.3 0.5)]
    (out out-bus (pan2 (* amp echo)))))



(defsynth overpad
  [in-trg 0
   in-trg-val 0
   in-freq 60
   in-freq-val 60
   in-amp 0.7
   in-amp-val 0.7
   in-attack 0.001
   in-attack-val 0.001
   in-decay 0.3
   in-decay-val 0.3
   in-sustain 0.99
   in-sustain-val 0.99
   in-release 2
   in-release-val 2
   in-gate-select 1
   in-gate-select-val 1
   ctrl-out 0
   out-bus 0]
  (let [gate       (in:kr in-trg)
        gate-val   (in:kr in-trg-val)
        trg-gate   (trig gate gate-val)
        trg-gate   (trig gate gate-val)
        gate       (select:kr (in:kr in-gate-select-val)  [trg-gate gate])
        note       (in:kr in-freq-val)
        amp        (in:kr in-amp-val)
        attack     (in:kr in-attack-val)
        decay      (in:kr in-decay-val)
        sustain    (in:kr in-sustain-val)
        release    (in:kr in-release-val)
        freq       note
        env        (env-gen (adsr attack decay sustain release) :gate gate)
        f-env      (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)) :gate gate)))
        bfreq      (/ freq 2)
        sig        (apply +
                          (concat (* 0.7 (sin-osc [bfreq (* 0.99 bfreq)]))
                                  (lpf (saw [freq (* freq 1.01)]) f-env)))
        audio      (* amp env sig)]
    (out out-bus (pan2 audio))))

(defsynth bass
  [in-trg 0
   in-trg-val 0
   in-freq 120
   in-freq-val 120
   in-t 0.6
   in-t-val 0.6
   in-amp 0.5
   in-amp-val 0.5
   ctrl-out 0
   out-bus 0 ]
  (let [gate (in:kr in-trg)
        freq (in:kr in-freq-val)
        t    (in:kr in-t-val )
        amp  (in:kr in-amp-val)
        env  (env-gen (perc 0.08 t) :gate gate)
        src  (saw [freq (* 0.98 freq) (* 2.015 freq)])
        src  (clip2 (* 1.3 src) 0.8)
        sub  (sin-osc (/ freq 2))
        filt (resonz (rlpf src (* 4.4 freq) 0.09) (* 2.0 freq) 2.9)]
    (out out-bus (pan2 (* env amp (fold:ar (distort (* 1.3 (+ filt sub))) 0.08))))
    ))

(defsynth daf-bass [in-trg 0
                    in-trg-val 0
                    in-freq 120
                    in-freq-val 120
                    in-gate-select 1
                    in-gate-select-val 1
                    in-amp 0.3
                    in-amp-val 0.3
                    ctrl-out 0
                    out-bus 0]
  (let [freq (in:kr in-freq-val)
        gate       (in:kr in-trg)
        gate-val   (in:kr in-trg-val)
        trg-gate   (trig gate gate-val)
        gate       (select:kr (in:kr in-gate-select-val)  [trg-gate gate])
        amp        (in:kr in-amp-val)
        harm       [1 1.01 2 2.02 3.5 4.01 5.501]
        harm       (concat harm (map #(* 2 %) harm))
        snd        (* 2 (distort (sum (sin-osc (* freq harm)))))
        snd        (+ snd (repeat 2 (sum (sin-osc (/ freq [1 2])))))
        env        (env-gen (adsr 0.001 0.2 0.9 0.25) :gate gate )]
    (out out-bus (pan2 (* amp snd env)))))


(defsynth grunge-bass
  [in-trg 0
   in-trg-val 0
   in-freq 48
   in-freq-val 48
   in-amp 0.5
   in-amp-val 0.5
   in-dur 0.1
   in-dur-val 0.1
   in-a 0.001
   in-a-val 0.001
   in-d 0.1
   in-d-val 0.1
   in-s 0.9
   in-s-val 0.9
   in-r 0.4
   in-r-val 0.4
   in-gate-select 1
   in-gate-select-val 1
   ctrl-out 0
   out-bus 0]
  (let [gate       (in:kr in-trg)
        gate-val   (in:kr in-trg-val)
        trg-gate   (trig gate gate-val)
        gate       (select:kr (in:kr in-gate-select-val)  [trg-gate gate])
        note       (in:kr in-freq-val)
        freq       note
        amp        (in:kr in-amp-val)
        dur        (in:kr in-dur-val)
        a          (in:kr in-a-val)
        d          (in:kr in-d-val)
        s          (in:kr in-s-val)
        r          (in:kr in-r-val)
        env        (env-gen (adsr a d s r) (line:kr 1 0 (+ a d dur r 0.1))
                            :gate gate)
        src        (saw [freq (* 0.98 freq) (* 1.015 freq)])
        src        (clip2 (* 1.3 src) 0.9)
        sub        (sin-osc (/ freq 2))
        filt       (resonz (rlpf src (* 8.4 freq) 0.29) (* 2.0 freq) 2.9)
        meat       (ring4 filt sub)
        sliced     (rlpf meat (* 2 freq) 0.1)
        bounced    (free-verb sliced 0.8 0.9 0.2)]
    (out out-bus (pan2 (* amp env bounced)))))


(defsynth vintage-bass
  [in-trg 0
   in-trg-val 0
   in-freq 40
   in-freq-val 40
   in-velocity 80
   in-velocity-val 80
   in-t 0.6
   in-t-val 0.6
   in-amp 1
   in-amp-val 1
   in-a 0.001
   in-a-val 0.001
   in-d 0.3
   in-d-val 0.3
   in-s 1
   in-s-val 1
   in-r 0.8
   in-r-val 0.8
   in-gate-select 1
   in-gate-select-val 1
   ctrl-out 0
   out-bus 0]
  (let [gate       (in:kr in-trg)
        gate-val   (in:kr in-trg-val)
        trg-gate   (trig gate gate-val)
        gate       (select:kr (in:kr in-gate-select-val)  [trg-gate gate])
        note       (in:kr in-freq-val)
        velocity   (in:kr in-velocity-val)
        t          (in:kr in-t-val)
        amp        (in:kr in-amp-val)
        a          (in:kr in-a-val)
        d          (in:kr in-d-val)
        s          (in:kr in-s-val)
        r          (in:kr in-r-val)
        freq       note
        sub-freq   (max 1 (- note 20))
        velocity   (/ velocity 127.0)
        sawz1      (* 0.275 (saw [freq (* 1.01 freq)]))
        sawz2      (* 0.75 (saw [(- freq 2) (+ 1 freq)]))
        sqz        (* 0.3 (pulse [sub-freq (- sub-freq 1)]))
        mixed      (* 5 (+ sawz1 sawz2 sqz))
        env        (env-gen (adsr a d s r) :gate gate)
        filt       (* env (moog-ff mixed (* velocity env (+ freq 200)) 2.2))]
    (out out-bus (pan2 (* amp filt)))))


(defsynth b3
  [in-trg 0
   in-trg-val 0
   in-freq 60
   in-freq-val 60
   in-amp 0.3
   in-amp-val 0.3
   in-a 0.01
   in-a-val 0.01
   in-d 3
   in-d-val 3
   in-s 1
   in-s-val 1
   in-r 0.3
   in-r-val 0.3
   in-gate-select 0
   in-gate-select-val 0
   ctrl-out 0
   out-bus 0]
  (let [gate       (in:kr in-trg)
        gate-val   (in:kr in-trg-val)
        trg-gate   (trig gate gate-val)
        gate       (select:kr (in:kr in-gate-select-val)  [trg-gate gate])
        note       (in:kr in-freq-val)
        amp        (in:kr in-amp-val)
        a          (in:kr in-a-val)
        d          (in:kr in-d-val)
        s          (in:kr in-s-val)
        r          (in:kr in-r-val)
        freq       note
        waves      (sin-osc [(* 0.5 freq)
                             freq
                             (* (/ 3 2) freq)
                             (* 2 freq)
                             (* freq 2 (/ 3 2))
                             (* freq 2 2)
                             (* freq 2 2 (/ 5 4))
                             (* freq 2 2 (/ 3 2))
                             (* freq 2 2 2)])
        snd        (apply + waves)
        env        (env-gen (adsr a d s r) :gate gate)]
    (out out-bus (pan2 (* env snd amp)))))


(defsynth ks1
  [in-trg 0
   in-trg-val 0
   in-freq 60
   in-freq-val 60
   in-amp 1
   in-amp-val 1
   in-dur 2
   in-dur-val 2
   in-decay 1
   in-decay-val 1
   in-coef 0.1
   in-coef-val 0.1
   ctrl-out 0
   out-bus 0]
  (let [gate       (in:kr in-trg)
        gate-val   (in:kr in-trg-val)
        note       (in:kr in-freq-val)
        amp        (in:kr in-amp-val)
        dur        (in:kr in-dur-val)
        decay      (in:kr in-decay-val)
        coef       (in:kr in-coef-val)
        freq       note
        noize      (* 0.8 (white-noise))
        dly        (/ 1.0 freq)
        plk        (pluck noize gate (/ 1.0 freq) dly
                          decay
                          coef)
        dist       (distort plk)
        filt       (rlpf dist (* 12 freq) 0.6)
        clp        (clip2 filt 0.8)
        env        (env-gen (perc 0.01 dur) :gate gate)
        reverb     (free-verb (* clp 1) 0.4 0.8 0.2)]
    (out out-bus (pan2 (* amp env reverb)))))

(defsynth bowed
  [in-trg 0
   in-trg-val 0
   in-freq 60
   in-freq-val 60
   in-velocity 80
   in-velocity-val 80
   in-amp 1
   in-amp-val 1
   in-bow-offset 0
   in-bow-offset-val 0
   in-bow-slope 0.5
   in-bow-slope-val 0.5
   in-bow-position 0.75
   in-bow-position-val 0.75
   in-vib-freq 6.127
   in-vib-freq-val 6.127
   in-vib-gain 0.2
   in-vib-gain-val 0.2
   in-gate-select 0
   in-gate-select-val 0
   in-attack 0.01
   in-attack-val 0.01
   in-decay 0.005
   in-decay-val 0.005
   in-sustain 0.01
   in-sustain-val 0.01
   in-release 2
   in-release-val 2
   ctrl-out 0
   out-bus 0]
  (let [gate         (in:kr in-trg)
        gate-val     (in:kr in-trg-val)
        trg-gate     (trig gate gate-val)
        gate         (select:kr (in:kr in-gate-select-val)  [trg-gate gate])
        note         (in:kr in-freq-val)
        velocity     (in:kr in-velocity-val)
        amp          (in:kr in-amp-val)
        bow-offset   (in:kr in-bow-offset-val)
        bow-slope    (in:kr in-bow-slope-val)
        bow-position (in:kr in-bow-position-val)
        vib-freq     (in:kr in-vib-freq-val)
        vib-gain     (in:kr in-vib-gain-val)
        attack       (in:kr in-attack-val)
        decay        (in:kr in-decay-val)
        sustain      (in:kr in-sustain-val)
        release      (in:kr in-release-val)
        freq         note
        velocity     (/ velocity 127)
        beta-ratio   (+ 0.027236 (* 0.2 bow-position))
        base-delay   (reciprocal freq)
        [fb1 fb2]    (local-in 2)
        vibrato      (* (sin-osc vib-freq) vib-gain)
        neck-delay   (+ (* base-delay (- 1 beta-ratio)) (* base-delay vibrato))
        neck         (delay-l fb1 0.05 neck-delay)
        nut-refl     (neg neck)
        bridge       (delay-l fb2 0.025 (* base-delay beta-ratio))
        string-filt  (one-pole (* bridge 0.95) 0.55)
        bridge-refl  (neg string-filt)
        adsr         (* amp (env-gen (adsr attack decay sustain release) :gate gate))
        string-vel   (+ bridge-refl nut-refl)
        vel-diff     (- adsr string-vel)
        slope        (- 5.0 (* 4 bow-slope))
        bow-table    (clip:ar (pow (abs (+ (* (+ vel-diff bow-offset) slope) 0.75 )) -4) 0 1)
        new-vel       (* vel-diff bow-table)]
   (local-out (+ [bridge-refl nut-refl] new-vel))
   (out out-bus (pan2 (resonz (* amp 10 bridge 0.5) 500 0.85)))))



;drum synths from https://github.com/overtone/overtone/blob/master/src/overtone/inst/drum.clj

(defsynth kick2
  [in-trg             0
   in-trg-val         0
   in-freq            50
   in-freq-val        50
   in-env-ratio       3
   in-env-ratio-val   3
   in-freq-decay      0.02
   in-freq-decay-val  0.02
   in-amp-decay       0.5
   in-amp-decay-val   0.5
   in-amp             1
   in-amp-val         1
   out-bus            0
   ctrl-out           0]
  (let [gate          (in:kr in-trg)
        freq          (in:kr in-freq-val)
        env-ratio     (in:kr in-env-ratio-val)
        freq-decay    (in:kr in-freq-decay-val)
        amp-decay     (in:kr in-amp-decay-val)
        amp           (in:kr in-amp-val)
        fenv          (* (env-gen (envelope [env-ratio 1] [freq-decay] :exp) :gate gate) freq)
        aenv          (env-gen (perc 0.005 amp-decay) :gate gate)]
    (out out-bus (pan2 (* amp (sin-osc fenv (* 0.5 Math/PI)) aenv)))))


(defsynth kick3 [in-trg             0
                in-trg-val         0
                in-freq            80
                in-freq-val        80
                in-amp             0.8
                in-amp-val         0.8
                in-mod-freq        60
                in-mod-freq-val    60
                in-mod-index       5
                in-mod-index-val   5
                in-sustain         0.4
                in-sustain-val     0.4
                in-noise           0.025
                in-noise-val       0.025
                out-bus            0
                ctrl-out           0]
  (let [gate               (in:kr in-trg)
        freq               (in:kr in-freq-val)
        amp                (in:kr in-amp-val)
        mod-freq           (in:kr in-mod-freq-val)
        mod-index          (in:kr in-mod-index-val)
        sustain            (in:kr in-sustain-val)
        noise              (in:kr in-noise-val)
        pitch-contour      (line:kr (* 2 freq) freq 0.02)
        drum               (lpf (sin-osc pitch-contour
                                         (sin-osc mod-freq (/ mod-index 1.3))) 1000)
        drum-env           (env-gen (perc 0.005 sustain) :gate gate)
        hit                (hpf (* noise (white-noise)) 500)
        hit                (lpf hit (line 6000 500 0.03))
        hit-env            (env-gen (perc 0.005 1) :gate gate)]
    (out out-bus (pan2 (* amp (+ (* drum drum-env) (* hit hit-env)))))))


(defsynth kick4
  [in-trg        0
   in-freq       80
   in-freq-val   80
   in-amp        0.3
   in-amp-val    0.3
   in-attack     0.001
   in-attack-val 0.001
   in-decay      0.4
   in-decay-val  0.4
   out-bus       0
   ctrl-out      0]
  (let [gate      (in:kr in-trg)
        freq      (in:kr in-freq-val)
        attack    (in:kr in-attack-val)
        decay     (in:kr in-decay-val)
        amp       (in:kr in-amp-val)
        env       (env-gen (perc attack decay) :gate gate)
        snd       (sin-osc freq (* Math/PI 0.5))
        snd       (* amp env snd)]
    (out out-bus (pan2 snd))))


(defsynth dub-kick
  [in-trg      0
   in-freq     80
   in-freq-val 80
   in-amp      1
   in-amp-val  1
   ctrl-out    0
   out-bus     0]
  (let [gate       (in:kr in-trg)
        freq       (in:kr in-freq-val)
        amp        (in:kr in-amp-val)
        cutoff-env (perc 0.001 1 freq -20)
        amp-env    (perc 0.001 1 1 -8)
        osc-env    (perc 0.001 1 freq -8)
        noiz       (lpf (white-noise) (+ (env-gen:kr cutoff-env :gate gate) 20))
        snd        (lpf (sin-osc (+ (env-gen:kr osc-env :gate gate) 20)) 200)
        mixed      (* (+ noiz snd) (env-gen amp-env :gate gate))]
    (out out-bus (pan2 (* amp mixed)))))

(defsynth dance-kick
  [in-trg        0
   in-freq       50.24
   in-freq-val   50.24
   in-amp        0.8
   in-amp-val    0.8
   in-attack     0.0001
   in-attack-val 0.0001
   in-decay      0.484
   in-decay-val  0.484
   in-fattack     0.0001
   in-fattack-val 0.0001
   in-fdecay      0.484
   in-fdecay-val  0.484
   ctrl-out      0
   out-bus       0]
  (let [gate       (in:kr in-trg)
        freq       (in:kr in-freq-val)
        attack     (in:kr in-attack-val)
        decay      (in:kr in-decay-val)
        fattack    (in:kr in-fattack-val)
        fdecay     (in:kr in-fdecay-val)
        amp        (in:kr in-amp-val)
        freq-env   (env-gen:kr (perc fattack fdecay))
        wave       (sin-osc (+ freq (* 8 freq freq-env)))
        env        (env-gen:kr (perc attack decay) :gate gate)
        src        (* env wave)
        dist       (clip2 (* 2 (tanh (* 3 (distort (* 1.5 src))))) 0.8)
        eq         (b-peak-eq dist 37.67 1 10.4)]
    (out out-bus (pan2 (* amp eq)))))

(defsynth dry-kick
  [in-trg        0
   in-freq       50.24
   in-freq-val   50.24
   in-amp        0.8
   in-amp-val    0.8
   in-attack     0.0001
   in-attack-val 0.0001
   in-decay      0.484
   in-decay-val  0.484
   ctrl-out      0
   out-bus       0]
  (let [gate       (in:kr in-trg)
        freq       (in:kr in-freq-val)
        attack     (in:kr in-attack-val)
        decay      (in:kr in-decay-val)
        amp        (in:kr in-amp-val)
        env        (env-gen (perc attack decay) :gate gate)
        snd        (mix (sin-osc [freq (* 2 freq) (- freq 15)] (* Math/PI 0.5)))
        snd        (* amp env snd)]
    (out out-bus (pan2 snd))))


(defsynth quick-kick
  [in-trg        0
   in-trg-val    0
   in-freq       50.24
   in-freq-val   50.24
   in-amp        0.1
   in-amp-val    0.1
   in-attack     0.0001
   in-attack-val 0.0001
   in-decay      0.1
   in-decay-val  0.1
   in-fattack     0.0001
   in-fattack-val 0.0001
   in-fdecay      0.484
   in-fdecay-val  0.484
   ctrl-out      0
   out-bus       0]
  (let [gate       (in:kr in-trg)
        gate-val   (in:kr in-trg-val)
        freq       (in:kr in-freq-val)
        attack     (in:kr in-attack-val)
        decay      (in:kr in-decay-val)
        fattack    (in:kr in-fattack-val)
        fdecay     (in:kr in-fdecay-val)
        amp        (in:kr in-amp-val)
        freq-env   (env-gen:kr (perc fattack fdecay) :gate gate)
        wave       (sin-osc (+ (* 0.5 freq) (* 14 freq freq-env)))
        env        (env-gen:kr (lin 0.01 0.01 decay 1 :exp) :gate gate) ;(x-line:kr 1 0 decay)
        src        (* env wave)
        dist       (clip2 (* 2 (tanh (* 3 (distort (* 1.5 src))))) 0.8)
        eq         (b-peak-eq dist 57.41 1 44)]
    (out out-bus (pan2 (* amp eq)))))

(defsynth hat1
  [in-trg     0
   in-trg-val 0
   in-amp     0.3
   in-amp-val 0.3
   in-t       0.3
   in-t-val   0.3
   in-low     6000
   in-low-val 6000
   in-hi      2000
   in-hi-val  2000
   ctrl-out   0
   out-bus    0]
  (let [gate      (in:kr in-trg)
        amp       (in:kr in-amp-val)
        t         (in:kr in-t-val)
        low       (in:kr in-low-val)
        hi        (in:kr in-hi-val)
        low       (lpf (white-noise) low)
        hi        (hpf low hi)
        env       (env-gen (lin 0 0 t 1 :linear) :gate gate)]
    (out out-bus (pan2 (* amp env hi)))))

(defsynth hat2
  [in-trg        0
   in-trg-val    0
   in-amp        0.3
   in-amp-val    0.3
   in-attack     0.001
   in-attack-val 0.001
   in-decay      0.1
   in-decay-val  0.1
   ctrl-out 0
   out-bus 0]
  (let [gate       (in:kr in-trg)
        amp        (in:kr in-amp-val)
        attack     (in:kr in-attack-val)
        decay      (in:kr in-decay-val)
        env        (env-gen (perc attack decay) :gate gate)
        noise      (white-noise)
        sqr        (* (env-gen (perc 0.01 0.04) :gate gate) (pulse 880 0.2))
        filt       (bpf (+ sqr noise) 9000 0.5)]
    (out out-bus (pan2 (* 0.5 amp env filt)))))

(defsynth soft-hat
  [in-trg        0
   in-trg-val    0
   in-freq       6000
   in-freq-val   6000
   in-amp        0.3
   in-amp-val    0.3
   in-attack     0.001
   in-attack-val 0.001
   in-decay      0.1
   in-decay-val  0.1
   ctrl-out 0
   out-bus 0 ]
  (let [gate       (in:kr in-trg)
        freq       (in:kr in-freq-val)
        amp        (in:kr in-amp-val)
        attack     (in:kr in-attack-val)
        decay      (in:kr in-decay-val)
        env        (env-gen (perc attack decay) :gate gate)
        noiz       (bpf (* amp (gray-noise)) freq 0.3)
        snd        (* noiz env)]
    (out out-bus (pan2 snd))))


(defsynth noise-hat
  [in-trg        0
   in-trg-val    0
   in-freq       6000
   in-freq-val   6000
   in-amp        0.3
   in-amp-val    0.3
   in-attack     0.1
   in-attack-val 0.1
   in-decay      0.001
   in-decay-val  0.001
   ctrl-out 0
   out-bus 0 ]
  (let [gate       (in:kr in-trg)
        freq       (in:kr in-freq-val)
        amp        (in:kr in-amp-val)
        attack     (in:kr in-attack-val)
        decay      (in:kr in-decay-val)
        env        (env-gen:kr (perc attack decay) :gate gate)
        noiz       (bpf (* amp (gray-noise))
                        (line freq 50 (* decay 0.5))
                        (*  1 ))
        snd (* 5 noiz env)]
    (out out-bus (pan2 snd))))

(defsynth bell-hat
  [in-trg        0
   in-trg-val    0
   in-freq       6000
   in-freq-val   6000
   in-amp        0.3
   in-amp-val    0.3
   in-attack     0.0001
   in-attack-val 0.0001
   in-decay      0.1
   in-decay-val  0.1
   ctrl-out 0
   out-bus 0]
  (let [gate       (in:kr in-trg)
        freq       (in:kr in-freq-val)
        amp        (in:kr in-amp-val)
        attack     (in:kr in-attack-val)
        decay      (in:kr in-decay-val)
        env        (env-gen (perc attack decay) :gate gate)
        noiz       (bpf (* amp (gray-noise)) (line freq 5 (* decay 0.5)) (+ env 0.1))
        wave       (* 0.1 env (mix (sin-osc [4000 6500 5000])))
        snd        (+ noiz wave)]
    (out out-bus (pan2 snd))))

(defsynth snare2
  [in-trg              0
   in-trg-val          0
   in-freq             405
   in-freq-val         405
   in-amp              1
   in-amp-val          1
   in-attack           0.005
   in-attack-val       0.005
   in-decay            0.1
   in-decay-val        0.1
   in-drum-amp         0.25
   in-drum-amp-val     0.25
   in-crackle-amp      40
   in-crackle-amp-val  40
   in-tightness        1000
   in-tightness-val    1000
   ctrl-out 0
   out-bus 0]
  (let [gate         (in:kr in-trg)
        freq         (in:kr in-freq-val)
        amp          (in:kr in-amp-val)
        attack       (in:kr in-attack-val)
        decay        (in:kr in-decay-val)
        drum-amp     (in:kr in-drum-amp-val)
        crackle-amp  (in:kr in-crackle-amp-val)
        tightness    (in:kr in-tightness-val)
        drum-env     (env-gen (perc attack decay) :gate gate)
        drum-osc     (mix (* drum-env (sin-osc [freq (* freq 0.53)])))
        drum-s3      (* drum-env (pm-osc (saw (* freq 0.85)) 184 (/ 0.5 1.3)))
        drum         (* drum-amp (+ drum-s3 drum-osc))
        noise        (* 0.1 (lf-noise0 20000))
        noise-env    (env-gen (perc attack decay) :gate gate)
        filtered     (* 0.5 (brf noise 8000 0.1))
        filtered     (* 0.5 (brf filtered 5000 0.1))
        filtered     (* 0.5 (brf filtered 3600 0.1))
        filtered     (* (brf filtered 2000 0.0001) noise-env)
        resonance    (* (resonz filtered tightness) crackle-amp)]
    (out out-bus (pan2 (* amp (+ drum resonance))))))

(defsynth snare3
  [in-trg              0
   in-trg-val          0
   in-freq             261.62
   in-freq-val         261.62
   in-amp              1
   in-amp-val          1
   in-attack           0.001
   in-attack-val       0.001
   in-decay            0.081
   in-decay-val        0.081
   ctrl-out            0
   out-bus             0]
  (let [gate         (in:kr in-trg)
        freq         (in:kr in-freq-val)
        amp          (in:kr in-amp-val)
        attack       (in:kr in-attack-val)
        decay        (in:kr in-decay-val)
        snd-env      (env-gen (perc attack decay) :gate gate)
        snd-env-b    (env-gen (perc attack (* decay 0.28)) :gate gate)
        snd          (* 0.1 (lpf (square
                                  (- freq (* freq snd-env 0.4) (* freq snd-env-b 0.05)))
                                 (* 2.5 freq)))
        amp-env      (env-gen (perc 0.001 (+ decay 0.036)) :gate gate)
        noise        (* 0.2 amp-env (pink-noise))
        snd          (rlpf (* amp amp-env (+ snd noise)) 10567 0.2)]
    (out out-bus (pan2 snd))))

(defsynth noise-snare
  [in-trg              0
   in-trg-val          0
   in-freq             1000
   in-freq-val         1000
   in-amp              1
   in-amp-val          1
   in-attack           0.00
   in-attack-val       0.00
   in-decay            0.1
   in-decay-val        0.1
   ctrl-out            0
   out-bus             0]
  (let [gate         (in:kr in-trg)
        freq         (in:kr in-freq-val)
        amp          (in:kr in-amp-val)
        attack       (in:kr in-attack-val)
        decay        (in:kr in-decay-val)
        env          (env-gen (perc attack decay) :gate gate)
        snd          (bpf (gray-noise) freq 3)]
    (out out-bus (pan2 (* snd env amp)))))

(defsynth tone-snare
  [in-trg              0
   in-trg-val          0
   in-freq             1000
   in-freq-val         1000
   in-amp              0.3
   in-amp-val          0.3
   ctrl-out            0
   out-bus             0]
  (let [gate         (in:kr in-trg)
        freq         (in:kr in-freq-val)
        amp          (in:kr in-amp-val)
        filterenv    (env-gen (lin 0 0.00 0.2) :gate gate)
        amp-env      (env-gen (lin 0 0 0.2) :gate gate)
        snd          (pulse 100)
        snd          (lpf snd (+ (* filterenv freq) 30))
        snap-env     (env-gen (lin 0 0 0.2) :gate gate)
        snap-osc     (bpf (hpf (white-noise) 500) 1500)]
    (out out-bus (pan2 (* amp (+ (* snd amp-env)
                                 (* snap-env snap-osc)))))))

(defsynth tom
  [in-trg              0
   in-trg-val          0
   in-freq             90
   in-freq-val         90
   in-amp              1
   in-amp-val          1
   in-attack           0.005
   in-attack-val       0.005
   in-decay            0.4
   in-decay-val        0.4
   in-mode-level       0.25
   in-mode-level-val   0.25
   in-timbre           1
   in-timbre-val       1
   in-stick-level      1
   in-stick-level-val  1
   ctrl-out            0
   out-bus             0]
  (let [gate         (in:kr in-trg)
        freq         (in:kr in-freq-val)
        amp          (in:kr in-amp-val)
        attack       (in:kr in-attack-val)
        decay        (in:kr in-decay-val)
        mode-level   (in:kr in-mode-level-val)
        timbre       (in:kr in-timbre-val)
        stick-level  (in:kr in-stick-level-val)
        env          (env-gen (perc attack decay) :gate gate)
        s1           (* 0.5 env (sin-osc (* freq 0.8)))
        s2           (* 0.5 env (sin-osc freq))
        s3           (* 5 env (sin-osc (saw (* 0.9 freq))
                                       (* (sin-osc (* freq 0.85))
                                          (/ timbre 1.3))))
        mix          (* mode-level (+ s1 s2 s3))
        stick        (* stick-level
                        (env-gen (perc 0.001 0.01) :gate gate)
                        (crackle 2.0))
        mix2         (* amp (+ mix stick))]
    (out out-bus (pan2 mix2))))

(defsynth haziti-clap
  [in-trg              0
   in-trg-val          0
   in-freq             80
   in-freq-val         80
   in-amp              1
   in-amp-val          1
   in-attack           0.036
   in-attack-val       0.036
   in-decay            1.884
   in-decay-val        1.884
   in-rq               0.08
   in-rq-val           0.08
   ctrl-out            0
   out-bus             0]
  (let [gate         (in:kr in-trg)
        freq         (in:kr in-freq-val)
        amp          (in:kr in-amp-val)
        attack       (in:kr in-attack-val)
        decay        (in:kr in-decay-val)
        rq           (in:kr in-rq-val)
        noiz         (white-noise)
        bfreq        (* 400 (abs (lf-noise0 freq)))
        filt         (* 4 (bpf (rhpf noiz 4064.78 rq) bfreq (* 1 rq)))
        env          (env-gen  (lin 0.001 1 decay 1 :exp) :gate gate)
        wave         (lf-tri (* (abs (lf-noise0:kr 699)) 4400))
        wenv         (env-gen (perc 0.00001 0.008) :gate gate)
        skip         (* wave wenv)]
    (out out-bus (pan2 (* amp (+ (* env filt) skip))))))
