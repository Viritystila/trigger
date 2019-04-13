(ns #^{:author "Mikael Reponen"}
  trigger.trigger
  (:use [overtone.live])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))


                                        ;State atoms
(defonce synthConfig (atom {}))

                                        ;groups
  (do
    (defonce main-g (group "main group")))

                                        ;base duration
(do
  (def base-dur (buffer 1))
  (buffer-write! base-dur [1]))


                                        ;Synthdefs
(defsynth base-trigger-synth [dur 1 out-bus 0] (out:kr out-bus (trig:kr (impulse:kr (/ 1  (dbufrd base-dur (dseries 0 1 INF) ))))))

(defsynth base-trigger-counter [base-trigger-bus-in 0 base-trigger-count-bus-out 0]
  (out:kr base-trigger-count-bus-out (pulse-count:kr (in:kr base-trigger-bus-in))))



(defsynth trigger-generator [base-trigger-bus-in 0
                            base-counter-bus-in 0
                            base-pattern-buffer-in 0
                            base-pattern-value-buffer-in 0
                            trigger-bus-out 0
                            trigger-value-bus-out 0]
  (let [base-trigger            (in:kr base-trigger-bus-in)
        base-counter            (in:kr base-counter-bus-in)
        pattern-buffer-id       (dbufrd base-pattern-buffer-in base-counter)
        pattern-value-buffer-id (dbufrd base-pattern-value-buffer-in base-counter)
        trg                     (t-duty:kr (* (dbufrd base-dur (dseries 0 1 INF) ) (dbufrd pattern-buffer-id (dseries 0 1 INF) 0))
                                           base-trigger
                                           (dbufrd pattern-buffer-id (dseries 0 1 INF) 0))
        pattern-item-value      (demand:kr trg base-trigger (dbufrd pattern-value-buffer-id (dseries 0 1 INF)))
        pattern-trg-value       (demand:kr trg base-trigger (dbufrd pattern-buffer-id (dseries 0 1 INF)))
        pattern-item-value      (select:kr (= 0.0 pattern-trg-value) [pattern-item-value (in:kr trigger-value-bus-out)])]
    (out:kr trigger-bus-out trg)
    (out:kr trigger-value-bus-out pattern-item-value)))


                                        ;Start

(defn start []
  (defonce base-trigger-bus (control-bus 1))
  (defonce base-trigger-dur-bus (control-bus 1))
   (control-bus-set! base-trigger-dur-bus 1)
  (buffer-write! base-dur [1])
  (def base-trigger (base-trigger-synth [:tail main-g] base-trigger-dur-bus base-trigger-bus))
  (defonce base-trigger-count-bus (control-bus 1))
  (def base-trigger-count (base-trigger-counter [:tail main-g] base-trigger-bus base-trigger-count-bus))




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
    (def tstbus (control-bus 1))))

(defsynth tstsin [in-trg 0 in-trg-val 0 f 200 out-bus 0] (let [trg (in:kr in-trg)
                                                               val (in:kr in-trg-val)
                                         env (env-gen (perc 0.01 0.01 1 0) :gate trg)
                                         src (* env (sin-osc (* f val)))]
                                     (out out-bus src)))



                                        ;Functions

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
                                   {:dur durs :val (flatten input)}) )


(defn generate-buffer-vector [field & new-buf-data] (let [size  (count new-buf-data)
                                                          new-buf-data (if (= 1 (count new-buf-data)) (nth new-buf-data 0) new-buf-data )
                                                          ;_ (println "gnb" (count new-buf-data))
                                                          ]
                                                    (loop [xv new-buf-data
                                                           result []]
                                                      (if xv
                                                        (let [x      (first xv)
                                                              x-out  (generate-durations x)
                                                              ;_ (println x-out)
                                        ;- (println (keyword field))
                                                              x-item  (field x-out)
                                                              x-item (remove zero? x-item)
                                                              ;x-item (concat [0] (vec x-item)) ;The triggering synth requires a single zero in the begininng to work as intended
                                                              x-size (count x-item)
                                                              x-buf  (buffer x-size)
                                                              _      (buffer-write-relay! x-buf (vec x-item))]
                                                          ;(println x)
                                                          (recur (next xv) (conj result x-buf))) result))))




;(trg "tstsin" tstsin [0 1 1 0 11 1 1 1 1 11 1 1 1 1  1 1 1 1  1 1 1 [ 1 1 1 1  1 1 1]] [1 1 1 1 1 1 1 ])

;(generate-durations [1 0 1 5])


;(remove zero? [1 2 0.0 3])

;(apply identity ([1 1 1]))
                                        ;
;(def bub  (generate-buffer-vector :val   ))

;(doseq [x bub] (println (vec (buffer-data x))))

;(concat [0] (1 2 3))
                                        ;pattern timing adjustments
(defn set-pattern-duration [dur] (control-bus-set! base-trigger-dur-bus 1)
                                  (buffer-write! base-dur [dur]))






                                        ;Synth triggering generation

(defprotocol synth-control
  (kill-synth [this])
  (kill-trg   [this])
  (swap-synth [this synth-name])
  (ctl-synth [this var value])
  (get-trigger-value-bus [this] ))

(defrecord synthContainer [pattern-name
                         group
                         out-bus
                         play-synth
                         triggers]
  synth-control
  (kill-synth [this] (kill (. this play-synth)))
  (kill-trg   [this] (kill (. this group)))
  (swap-synth [this synth-name] (println "not implemented"))
  (ctl-synth [this var value] (ctl (. this play-synth) var value)))


(defprotocol trigger-control
  (kill-trg-group [this]))

(defrecord triggerContainer [control-key
                           control-val-key
                           group
                           play-synth
                           trigger-bus
                           trigger-value-bus
                           trigger-synth
                           pattern-buf
                           patter-value-buf]
  trigger-control
  (kill-trg-group [this] (do (kill (. this group)))))

(defn create-synth-config [pattern-name synth-name] (let [out-bus      0
                                                          synth-group  (group pattern-name :after main-g)
                                                          play-synth   (synth-name  [:tail synth-group] )
                                                          triggers     {}]
                                                      (synthContainer. pattern-name synth-group out-bus play-synth triggers)))


(defn create-trigger [control-key
                      control-val-key
                      synth-name
                      pattern-group
                      pattern-vector
                      pattern-value-vector]
  (let [trig-bus             (control-bus 1)
        trig-val-bus         (control-bus 1)
        buf-size             (count pattern-vector)
        pattern-id-buf       (buffer buf-size)
        pattern-value-id-buf (buffer buf-size)
        _                    (buffer-write! pattern-id-buf       (vec (map (fn [x] (buffer-id x)) pattern-vector)))
        _                    (buffer-write! pattern-value-id-buf (vec (map (fn [x] (buffer-id x)) pattern-value-vector)))
        trig-group           (group (str control-key) :after pattern-group)
        trig-synth           (trigger-generator [:tail trig-group]
                                                base-trigger-bus
                                                base-trigger-count-bus
                                                pattern-id-buf
                                                pattern-value-id-buf
                                                trig-bus
                                                trig-val-bus)]
        (ctl synth-name  control-key trig-bus control-val-key  trig-val-bus  )
    (triggerContainer. control-key control-val-key trig-group synth-name trig-bus
                       trig-val-bus  trig-synth  pattern-vector pattern-value-vector)))



(defn t [input] (let [pattern-name      (:pn input)
                      pattern-name-key  (keyword pattern-name)
                      synth-name        (:sn input)
                      control-pair      (first (dissoc (dissoc input :pn) :sn))
                      control-key       (first control-pair)
                      control-val-key   (keyword (str (name control-key) "-val"))
                      control-pattern   [(last control-pair)]
                      pattern-status    (pattern-name-key @synthConfig)
                      trig-pattern      (generate-buffer-vector :dur control-pattern )
                      val-pattern       (generate-buffer-vector :val control-pattern )
                      synth-container   (pattern-name-key @synthConfig)
                      pattern-group     (:group synth-container)
                      triggers          (:triggers synth-container)
                      play-synth        (:play-synth synth-container)
                      trigger           (create-trigger control-key
                                                        control-val-key
                                                        play-synth
                                                        pattern-group
                                                        trig-pattern
                                                        val-pattern
                                                        )
                      triggers          (assoc triggers control-key trigger)
                      synth-container   (assoc synth-container :triggers triggers)
                      ]
                  (swap! synthConfig assoc pattern-name-key synth-container)
                  (dissoc input control-key)))

;input as hashmap {:pn :sn}
(defn trg [input] (let [pattern-name      (:pn input)
                        pattern-name-key  (keyword pattern-name)
                        synth-name        (:sn input)
                        pattern-status    (pattern-name-key @synthConfig)]
                    (if  (= nil pattern-status)
                      (do (println "Synth created") (swap! synthConfig assoc pattern-name-key (create-synth-config pattern-name  synth-name)))
                      (do (println "Synth exits")))
                    input ))


(defn stop-pattern [pattern-name] (let [pattern-name-key      (keyword pattern-name)
                                    pattern-status        (pattern-name-key @synthConfig)]
                                (println pattern-status)
                                (if (some? pattern-status) (do (kill-trg pattern-status)
                                                               (swap! synthConfig dissoc pattern-name-key) ) )))








(start)

;(map (fn [x] (buffer-id x)) [b1 b2])

;(buffer-id b1)
