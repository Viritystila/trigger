(ns #^{:author "Mikael Reponen"}
  trigger.trigger
  (:use [overtone.live])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))


                                        ;State atoms
(defonce synthConfig (atom {}))

                                        ;groups
  (do
    (defonce main-g (group "main group")))

                                        ;Synthdefs
(defsynth baseTrigger [dur 1 out-bus 0] (out:kr out-bus (trig:kr (impulse:kr (/ 1 (in:kr dur))))))

(defsynth baseTriggerCounter [base-trigger-bus-in 0 base-trigger-count-bus-out 0]
  (out:kr base-trigger-count-bus-out (pulse-count:kr (in:kr base-trigger-bus-in))))


(defsynth triggerGenerator [base-trigger-bus-in 0
                            base-counter-bus-in 0
                            base-pattern-buffer-in 0
                            trigger-bus-out 0]
  (let [base-trigger        (in:kr base-trigger-bus-in)
            base-counter        (in:kr base-counter-bus-in)
        pattern-buffer-id   (dbufrd base-pattern-buffer-in base-counter)
        pattern_item        (dbufrd pattern-buffer-id (dseries 0 1 INF) 0)
        trg                 (t-duty:kr  (dbufrd pattern-buffer-id (dseries 0 1 INF) 0)  base-trigger  pattern_item)]
    (out:kr trigger-bus-out trg)))


(defn start []
  (defonce base-trigger-bus (control-bus 1))
  (defonce base-trigger-dur-bus (control-bus 1))
  (control-bus-set! base-trigger-dur-bus 1)
  (def base-trigger (baseTrigger [:tail main-g] base-trigger-dur-bus base-trigger-bus))
  (defonce base-trigger-count-bus (control-bus 1))
  (def base-trigger-count (baseTriggerCounter [:tail main-g] base-trigger-bus base-trigger-count-bus))


  )





                                        ;pattern testing

(do
  (def b1 (buffer 9))
  (buffer-write! b1 [0 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8])
  (def b2 (buffer 5))
  (buffer-write! b2 [0 1/4 1/4 1/4 1/4])
  (def b3 (buffer 3))
  (buffer-write! b3 [0 1/2 1/2])
  (def b4 (buffer 4))
  (buffer-write! b4 [0 1/3 1/3 1/3] )
  (def b5 (buffer 17))
  (buffer-write! b5 [0 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16])


  (def bp (buffer 5))
  (buffer-write! bp [(buffer-id b1) (buffer-id b2) (buffer-id b3) (buffer-id b4) (buffer-id b5)])
  (def bps (buffer 5))
  (buffer-write! bps [5 3 4 17 9])
  (def tstbus (control-bus 1)))

(defsynth tstsin [trig-in 0 f 200 out-bus 0] (let [trg (in:kr trig-in)
                                         env (env-gen (perc 0.01 0.01 1 0) :gate trg)
                                         src (* env (sin-osc f))]
                                     (out out-bus src)))


;(def tsttriggen (triggerGenerator base-trigger-bus base-trigger-count-bus bp tstbus))

                                        ;(def tstsin1 (tstsin tstbus))
;(kill tstsin1)

;(vec (buffer-data bp))

                                        ;Functions

                                        ;Synth triggering generation

(defprotocol synth-control
  (kill-synth [this])
  (kill-trg   [this])
  (swap-synth [this synth-name])
  (ctl-synth [this var value])
  (set-trigger-bus [thus trigger-bus-new]))

(deftype synthContainer [pattern-name
                         group
                         ^:volatile-mutable trigger-bus
                         ^:volatile-nutable out-bus
                         ^:volatile-mutable trigger-synth
                         ^:volatile-mutable play-synth
                         ^:volatile-mutable pattern-id-buf]
  synth-control
  (kill-synth [this] (kill (. this play-synth)))
  (kill-trg   [this] (kill (. this group)))
  (swap-synth [this synth-name] (println "not implemented"))
  (ctl-synth [this var value] (ctl (. this play-synth) var value))
  (set-trigger-bus [this trigger-bus-new] (set! trigger-bus trigger-bus-new ) (ctl (. this play-synth) :trig-in trigger-bus-new)))


(defn createSynthConfig [pattern-name synth-name pattern-id-buf] (let [trig-bus     (control-bus 1)
                                                                       out-bus      0
                                                                       synth-group  (group pattern-name)
                                                                       trig-synth   (triggerGenerator [:tail synth-group] base-trigger-bus base-trigger-count-bus pattern-id-buf trig-bus)
                                                                       play-synth   (synth-name  [:tail synth-group]  :trig-in trig-bus)]
                                                                   (synthContainer. pattern-name synth-group trig-bus out-bus trig-synth play-synth pattern-id-buf)))



(defn trg [pattern-name synth-name pattern] (let [pattern-name-key       (keyword pattern-name)
                                                  pattern-status    (pattern-name-key @synthConfig)
                                                  pattern-id-buf    [14.0 15.0 16.0 17.0 18.0]
                                                  pidb              (buffer (count pattern-id-buf))
                                                  _                 (buffer-write! pidb pattern-id-buf)]
                                              (if  (= nil pattern-status)
                                                (do (println "Synth created") (swap! synthConfig assoc pattern-name-key (createSynthConfig pattern-name
                                                                                                                                           synth-name
                                                                                                                                           pidb)) )
                                                (do (println "Synth exits")))))

(defn stop-pattern [pattern-name] (let [pattern-name-key      (keyword pattern-name)
                                    pattern-status        (pattern-name-key @synthConfig)]
                                (println pattern-status)
                                (if (some? pattern-status) (do (kill-trg pattern-status)
                                                               (swap! synthConfig dissoc pattern-name-key) ) )))

                                        ;pattern generation functions
(defn trigger-dur [dur] (if (= dur 0) 0 1) )

(defn traverse-vector ([input-array] (let [input-vec input-array
                                        ;_ (println input-vec)
                                           result []]
                                      (if true ;(vector? input-vec)
                                        (loop [xv (seq input-vec)
                                               result []]
                                          (if xv
                                            (let [;_ (println xv)
                                                   length (count input-vec)
                                                  x (first xv)]
                                               (if (vector? x) (recur (next xv) (conj result (traverse-vector x length)))
                                                   (recur (next xv) (conj result (/ 1 length 1))))) result)))))
  ([input-array bl] (let [input-vec input-array
                                        ;_ (println bl)
                          ]
                      (if (vector? input-vec)
                         (loop [xv (seq input-vec)
                                result []]
                           (if xv
                             (let [length (count input-vec)
                                   x (first xv)]
                               (if (vector? x) (recur (next xv) (conj result (traverse-vector x (* bl length))))
                                   (recur (next xv) (conj result (/ 1 length bl))))) result))))))


(defn sum-zero-durs [idxs input-vector full-durs] (loop [xv (seq idxs)
                                                       sum 0]
                                                  (if xv
                                                    (let [x       (first xv)
                                                          zero-x  (nth input-vector x )
                                                          dur-x   (nth full-durs x)]
                                                      (println zero-x)
                                                      (println dur-x)
                                                      (if (= zero-x 0) (do (recur (next xv) (+ dur-x sum))) sum)) sum)))


(defn adjust-duration [input-vector input-original] (let [length   (count input-vector)
                                                         full-durs input-vector
                                        ;_ (println full-durs)
                                                         input-vector (into [] (map * input-vector input-original))
                                                         idxs (vec (range length))]
                                                     (loop [xv (seq idxs)
                                                            result []]
                                                         (if xv
                                                          (let [xidx      (first xv)
                                                                nidx      (mod (+ 1 xidx) length)
                                                                opnext    (nth input-vector nidx)
                                                                op        (nth input-vector xidx)
                                                                vec-ring  (flatten (conj (subvec idxs nidx) (subvec idxs 0 nidx )))
                                        ;_  (println (subvec input-vector nidx))
                                        ;_ (println (countZeros (subvec input-vector nidx)))
                                                                op      (if (and (not= 0 op) ( = 0 opnext)) (+ op (sum-zero-durs vec-ring input-vector full-durs)) op)]
                                                            (recur (next xv) (conj result op))) result))))

(defn generate-durations [input] (let [mod-input (vec (map trigger-dur (vec (flatten input))))
                                      durs  (traverse-vector input)
                                      durs  (into [] (flatten durs))
                                      durs  (adjust-duration durs (vec (flatten mod-input)))]
                                        ;(println durs)
                                  durs) )

(defn set-buffer [synth-in buf new-buf-data] (let [size         (count new-buf-data)
                                                   new-buf      (buffer size) ]
                                               (buffer-write-relay! new-buf new-buf-data)
                                                  (ctl synth-in :dur-buffer-in new-buf)
                                                  (buffer-free buf)
                                                  new-buf
                                                  ))

(defn set-buffer2 [synth-in buf & new-buf-data] (let [;new-buf-data (vec (concat new-buf-data))
                                                      new-buf-data (vec (flatten (vec new-buf-data)))
                                                      size         (count new-buf-data)
                                                      new-buf      (buffer size) ]
                                                  (buffer-write-relay! new-buf new-buf-data)
                                                  (ctl synth-in :dur-buffer-in new-buf)
                                                  (buffer-free buf)
                                                  new-buf
                                                  ))

(defn generate-dur-buffer-vector [& new-buf-data] (let [size  (count new-buf-data)]
                                                    (loop [xv new-buf-data
                                                           result []]
                                                      (if xv
                                                        (let [x      (first xv)
                                                              x-durs (generate-durations x)
                                                              x-size (count x-durs)
                                                              x-buf  (buffer x-size)
                                                              _      (buffer-write-relay! x-buf (vec x-durs))]
                                                          ;(println x)
                                                          (recur (next xv) (conj result x-buf))) result))))

;(def bub  (generate-dur-buffer-vector [1 0 1 0] [1 1 1 1] [3 3 3] ))

                                        ;(doseq [x bub] (println (vec (buffer-data x))))

(control-bus)
