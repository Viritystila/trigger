(ns #^{:author "Mikael Reponen"}
  trigger.trg_fx
  (:use [overtone.core]))

;Mostly adapted from https://github.com/overtone/overtone/blob/master/src/overtone/studio/fx.clj


(defsynth trg-fx-noise-gate
  "A noise gate only lets audio above a certain amplitude threshold through.  Often used to filter out hardware circuit noise or unwanted background noise."
  [bus-in 0
   in-threshold 0.4
   in-slope-below 1
   in-slope-above 0.1
   in-clamp-time 0.01
   in-relax-time 0.1
   in-threshold-val 0.4
   in-slope-below-val 1
   in-slope-above-val 0.1
   in-clamp-time-val 0.01
   in-relax-time-val 0.1
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [src (in bus-in)
        threshold (in:kr in-threshold-val)
        slope-below (in:kr in-slope-below-val)
        slope-above (in:kr in-slope-above-val)
        clamp-time (in:kr in-clamp-time-val)
        relax-time (in:kr in-relax-time-val)
        sig (compander src src threshold
                    slope-below slope-above
                    clamp-time relax-time)
        snd (select (in:kr in-out-select-val) [sig src])]
    (replace-out out-bus (pan2 snd))))

(defsynth trg-fx-compressor
  "A compressor clamps audio signals above an amplitude threshold down, compressing the dynamic range.  Used to normalize a poppy sound so that the amplitude is more consistent, or as a sound warping effect.  The clamp time determines the delay from when the signal is detected as going over the threshold to when clamping begins, and the slope determines the rate at which the clamp occurs."
  [bus-in 0
   in-threshold 0.2
   in-slope-below 1
   in-slope-above 0.5
   in-clamp-time 0.01
   in-relax-time 0.01
   in-threshold-val 0.2
   in-slope-below-val 1
   in-slope-above-val 0.5
   in-clamp-time-val 0.01
   in-relax-time-val 0.01
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [src (in bus-in)
        threshold (in:kr in-threshold-val)
        slope-below (in:kr in-slope-below-val)
        slope-above (in:kr in-slope-above-val)
        clamp-time (in:kr in-clamp-time-val)
        relax-time (in:kr in-relax-time-val)
        sig        (compander src src threshold
                              slope-below slope-above
                              clamp-time relax-time)
        snd (select (in:kr in-out-select-val) [sig src])]
    (replace-out out-bus (pan2 snd))))


(defsynth trg-fx-limiter
  "A limiter sets a maximum threshold for the audio amplitude, and anything above this threshold is quickly clamped down to within it."
  [bus-in 0
   in-threshold 0.2
   in-slope-below 1
   in-slope-above 0.1
   in-clamp-time 0.01
   in-relax-time 0.01
   in-threshold-val 0.2
   in-slope-below-val 1
   in-slope-above-val 0.1
   in-clamp-time-val 0.01
   in-relax-time-val 0.01
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [src (in bus-in)
        threshold (in:kr in-threshold-val)
        slope-below (in:kr in-slope-below-val)
        slope-above (in:kr in-slope-above-val)
        clamp-time (in:kr in-clamp-time-val)
        relax-time (in:kr in-relax-time-val)
        sig        (compander src src threshold
                              slope-below slope-above
                              clamp-time relax-time)
        snd (select (in:kr in-out-select-val) [sig src])]
    (replace-out out-bus (pan2 snd))))


(defsynth trg-fx-sustainer
  [bus-in 0
   in-threshold 0.2
   in-slope-below 1
   in-slope-above 0.5
   in-clamp-time 0.01
   in-relax-time 0.01
   in-threshold-val 0.2
   in-slope-below-val 1
   in-slope-above-val 0.5
   in-clamp-time-val 0.01
   in-relax-time-val 0.01
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [src (in bus-in)
        threshold (in:kr in-threshold-val)
        slope-below (in:kr in-slope-below-val)
        slope-above (in:kr in-slope-above-val)
        clamp-time (in:kr in-clamp-time-val)
        relax-time (in:kr in-relax-time-val)
        sig        (compander src src threshold
                              slope-below slope-above
                              clamp-time relax-time)
        snd (select (in:kr in-out-select-val) [sig src])]
    (replace-out out-bus (pan2 snd))))



(defsynth trg-fx-freeverb
  "Uses the free-verb ugen."
  [bus-in 0
   in-wet-dry 0.5
   in-room-size 0.5
   in-dampening 0.5
   in-wet-dry-val 0.5
   in-room-size-val 0.5
   in-dampening-val 0.5
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [src   (in bus-in)
        wet-dry (in:kr in-wet-dry-val)
        room-size (in:kr in-room-size-val)
        dampening (in:kr in-dampening-val)
        verbed (free-verb src wet-dry room-size dampening)
        snd (select (in:kr in-out-select-val) [(* 1.4 verbed) src])]
    (replace-out out-bus (pan2 snd))))

(defsynth trg-fx-reverb
  "Implements Schroeder reverb using delays."
  [bus-in 0
   in-sig-a 0.4
   in-sig-b 0.37
   in-sig-c 0.333
   in-sig-d 0.3
   in-del-a 101
   in-del-b 143
   in-del-c 165
   in-del-d 177
   in-sig-a-val 0.4
   in-sig-b-val 0.37
   in-sig-c-val 0.333
   in-sig-d-val 0.3
   in-del-a-val 101
   in-del-b-val 143
   in-del-c-val 165
   in-del-d-val 177
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [input (in bus-in)
        sig-a (in:kr in-sig-a-val)
        sig-b (in:kr in-sig-b-val)
        sig-c (in:kr in-sig-c-val)
        sig-d (in:kr in-sig-d-val)
        del-a (in:kr in-del-a-val)
        del-b (in:kr in-del-b-val)
        del-c (in:kr in-del-c-val)
        del-d (in:kr in-del-d-val)
        delrd (local-in 4)
        output (+ input [(first delrd) (second delrd)])
        sig [(+ (first output) (second output)) (- (first output) (second output))
             (+ (nth delrd 2) (nth delrd 3)) (- (nth delrd 2) (nth delrd 3))]
        sig [(+ (nth sig 0) (nth sig 2)) (+ (nth sig 1) (nth sig 3))
             (- (nth sig 0) (nth sig 2)) (- (nth sig 0) (nth sig 2))]
        sig (* sig [sig-a sig-b sig-c sig-c])
        deltimes (- (* [del-a del-b del-c del-d] 0.001) (control-dur))
        lout (local-out (delay-c sig deltimes deltimes))
        snd (select (in:kr in-out-select-val) [output input])]
    (replace-out out-bus snd)))


(defsynth trg-fx-echo
  [bus-in 0
   in-delay-time 0.4
   in-delay-time-val 0.4
   in-decay-time 2.0
   in-decay-time-val 2.0
   in-amp 0.1
   in-amp-val 0.1
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [src          (in bus-in)
        max-delay    1
        delay-time   (in:kr in-delay-time-val)
        decay-time   (in:kr in-decay-time-val)
        amp          (in:kr in-amp-val)
        echo         (comb-n src max-delay delay-time decay-time)
        sig          (+ (* amp echo) src)
        snd (select (in:kr in-out-select-val) [sig src])]
    (replace-out out-bus (pan2 snd 0))))



(defsynth trg-fx-chorus
  [bus-in 0
   in-rate 0.002
   in-depth 0.01
   in-rate-val 0.002
   in-depth-val 0.01
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [src (in bus-in)
        rate (in:kr in-rate-val)
        depth (in:kr in-depth-val)
        dub-depth (* 2 depth)
        rates [rate (+ rate 0.001)]
        osc (+ dub-depth (* dub-depth (sin-osc:kr rates)))
        dly-a (delay-l src 0.3 osc)
        sig (apply + src dly-a)
        snd (select (in:kr in-out-select-val) [(* 0.3 sig) src])]
    (replace-out out-bus (pan2 snd))))

(defsynth trg-fx-distortion
  [bus-in 0
   in-boost 4
   in-level 0.01
   in-boost-val 4
   in-level-val 0.01
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [src (in bus-in)
        boost (in:kr in-boost-val)
        level (in:kr in-level-val)
        snd    (distort (* boost (clip2 src level)))
        snd (select (in:kr in-out-select-val) [snd src])
        out-bus 0]
    (replace-out out-bus (pan2 snd))))

(defsynth trg-fx-distortion2
  [bus-in 0
   in-amount 0.5
   in-amount-val 0.5
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [src (in bus-in)
        amount (in:kr in-amount-val)
        k (/ (* 2 amount) (- 1 amount))
        snd (/ (* src (+ 1 k)) (+ 1 (* k (abs src))))
        snd (select (in:kr in-out-select-val) [snd src])]
    (replace-out out-bus (pan2 snd))))

(defsynth trg-fx-bitcrusher
  [bus-in 0
   in-bits 32
   in-bits-val 32
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [src (in bus-in)
        bits (in:kr in-bits-val)
        resolution (/ (*  (- bits 1)  (- bits 1)) 2)
        crushed (floor (/ (+ 0.5 (* src resolution)) resolution))
        snd (select (in:kr in-out-select-val) [crushed src])]
    (replace-out out-bus (pan2 snd))))

(defsynth trg-fx-tubescreamer
  [bus-in 0
   in-hi-freq 720.484
   in-low-freq 723.431
   in-hi-freq2 1
   in-gain 4
   in-threshold 0.4
   in-hi-freq-val 720.484
   in-low-freq-val 723.431
   in-hi-freq2-val 1
   in-gain-val 4
   in-threshold-val 0.4
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [src (in bus-in)
        hi-freq (in:kr in-hi-freq-val)
        low-freq (in:kr in-low-freq-val)
        hi-freq2 (in:kr in-hi-freq2-val)
        gain (in:kr in-gain-val)
        threshold (in:kr in-threshold-val)
        f1 (* (hpf src hi-freq) gain)
        f2 (lpf (clip2 f1 threshold) low-freq)
        f3 (hpf f2 hi-freq2)
        snd (select (in:kr in-out-select-val) [f3 src])]
    (replace-out out-bus (pan2 snd))))



(defsynth trg-fx-rlpf
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


(defsynth trg-fx-rhpf
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
        snd  (rhpf src cutoff res)
        snd (select (in:kr in-out-select-val) [snd src])]
    (replace-out out-bus (pan2 snd))))


(defsynth trg-fx-feedback
  [bus-in 0
   in-delay-t 0.5
   in-decay 0.5
   in-delay-t-val 0.5
   in-decay-val 0.5
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [input (in bus-in)
        delay-t (in:kr in-delay-t-val)
        decay   (in:kr in-decay-val)
        fb-in (local-in 1)
        snd (* decay (leak-dc (delay-n fb-in 4 (min 4 delay-t))))
        snd (+ input snd)
        fb-out (local-out snd)
        snd (limiter snd 0.8)
        snd (select (in:kr in-out-select-val) [snd input])]
    (replace-out out-bus (pan2 snd))))

(defsynth trg-fx-feedback-distortion
  [bus-in 0
   in-delay-t 0.5
   in-decay 0.5
   in-delay-t-val 0.5
   in-decay-val 0.5
   in-noise-rate 0.5
   in-boost 1.1
   in-noise-rate-val 0.5
   in-boost-val 1.1
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [delay-t (in:kr in-delay-t-val)
        decay   (in:kr in-decay-val)
        noise-rate (in:kr in-noise-rate-val)
        boost    (in:kr in-boost-val)
        noiz (mul-add (lf-noise0:kr noise-rate) 2 2.05)
        input (in bus-in)
        fb-in (local-in 1)
        snd (* boost (delay-n fb-in 4 noiz))
        snd (+ input (leak-dc snd))
        snd (clip:ar (distort snd) 0 0.9)
        fb-out (local-out (* decay snd))
        snd (select (in:kr in-out-select-val) [snd input])]
    (replace-out out-bus (pan2 snd))))


(defsynth trg-fx-pitch-shift
  "A pitch shifter"
  [bus-in 0
   in-pitch-ratio 1.0
   in-pitch-dispersion 0.0
   in-time-dispersion 0.0
   in-pitch-ratio-val 1.0
   in-pitch-dispersion-val 0.0
   in-time-dispersion-val 0.1
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [src               (in bus-in)
        pitch-ratio       (in:kr in-pitch-ratio-val)
        pitch-dispersion  (in:kr in-pitch-dispersion-val)
        time-dispersion   (in:kr in-time-dispersion-val)
        window-cize       0.05
        sig               (pitch-shift src window-cize pitch-ratio pitch-dispersion time-dispersion)
        snd (select (in:kr in-out-select-val) [sig src])]
    (replace-out out-bus (pan2 snd))))
