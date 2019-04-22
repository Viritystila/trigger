(ns #^{:author "Mikael Reponen"}
  trigger.trigger
  (:use [overtone.live]
        [clojure.data])
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [trigger.synths]))


                                        ;State atoms
(defonce synthConfig (atom {}))
(defonce bufferPool (atom {}))
                                        ;groups
  (do
    (defonce main-g (group "main group")))

                                        ;base duration
(do
  (def base-dur (buffer 1))
  (buffer-write! base-dur [1]))


                                        ;Synthdefs

(defsynth base-trigger-synth [dur 1 out-bus 0] (out:kr out-bus (t-duty:kr  (dbufrd base-dur (dseries 0 1 INF) ) 0 10 )))


(defsynth base-trigger-counter [base-trigger-bus-in 0 base-trigger-count-bus-out 0]
  (out:kr base-trigger-count-bus-out (pulse-count:kr (in:kr base-trigger-bus-in))))

(def dbg (control-bus 1))
(def dgb2 (control-bus 1))

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
        pattern-first-value     (demand:kr base-trigger base-trigger (dbufrd pattern-buffer-id (dseries 0 1 1) 0) )
        pattern-value-start-idx (select:kr (= 0.0 pattern-first-value) [0 1])
                                        ; Depending on if a spacer trigger exists or not in the first index of a buffer,
                                        ;this value needs to be either 1 or 0 in order to play the buffer as intended.
        trg                     (t-duty:kr (* (dbufrd base-dur (dseries 0 1 INF) ) (dbufrd pattern-buffer-id (dseries 0 1 INF) 0))
                                           base-trigger
                                           (dbufrd pattern-buffer-id (dseries 0 1 INF) 0))
        pattern-item-value      (demand:kr trg base-trigger (dbufrd pattern-value-buffer-id (dseries pattern-value-start-idx 1 INF)))
        pattern-trg-value       (demand:kr trg base-trigger (dbufrd pattern-buffer-id (dseries 0 1 INF)))
        cntr  (pulse-count:kr trg base-trigger)
        trg  (select:kr (= 0 cntr) [trg 0])
        ;_ (out:kr dbg pattern-value-start-idx)
        ;_ (out:kr dgb2 pattern-item-value)
        ]
    (out:kr trigger-value-bus-out pattern-item-value)
    (out:kr trigger-bus-out trg)))

;(remove-watch dm :dm)

(defsynth tstsin [in-trg 0 in-trg-val 0 in-attack 0 in-attack-val 0 f 200 out-bus 0] (let [trg (in:kr in-trg)
                                                                                           val (in:kr in-trg-val)
                                                                                           env (env-gen (perc (in:kr in-attack-val) 0.01 1 0) :gate trg)
                                                                                           src (* env (sin-osc (* f val)))]
                                                                                       ;(out:kr dbg (in:kr in-attack-val))
                                                                                       (out out-bus src)))

                                        ;Buffer pool functions
(defn store-buffer [buf] (let [size      (buffer-size buf)
                               size-key  (keyword (str size))
                               pool      @bufferPool
                               pool      (update pool size-key (fnil concat []) [buf])]
                           (reset! bufferPool pool)))

(defn retrieve-buffer [size] (let [size-key      (keyword (str size))
                                   pool          @bufferPool
                                   buffers-left  (and (contains? pool size-key) (< 0 (count (size-key pool))))
                                   first-buf     (first (size-key pool))
                                   rest-buf      (rest  (size-key pool))
                                   pool          (assoc pool size-key  rest-buf )]
                               (if buffers-left
                                 (do (reset! bufferPool pool) first-buf)
                                 (do (buffer size)))))


                                        ;Start



(defn start-trigger []
  (def base-trigger-bus (control-bus 1))
  (def base-trigger-dur-bus (control-bus 1))
  (control-bus-set! base-trigger-dur-bus 1)
  (buffer-write! base-dur [1])
  (def base-trigger (base-trigger-synth [:tail main-g] base-trigger-dur-bus base-trigger-bus))
  (def base-trigger-count-bus (control-bus 1))
  (def base-trigger-count (base-trigger-counter [:tail main-g] base-trigger-bus base-trigger-count-bus))
  (pmap (fn [x] (pmap (fn [y] (store-buffer (buffer (+ x 1))) ) (range 20) )) (range 30))
  (println "trigger initialized"))


                                        ;Pattern generation functions
(defn trigger-dur [dur] (if (= dur 0) 0 1) )

(defn traverse-vector ([input-array] (let [input-vec input-array
                                           result []]
                                      (if true
                                        (loop [xv (seq input-vec)
                                               result []]
                                          (if xv
                                            (let [ length (count input-vec)
                                                  x (first xv)]
                                               (if (vector? x) (recur (next xv) (conj result (traverse-vector x length)))
                                                   (recur (next xv) (conj result (/ 1 length 1))))) result)))))
  ([input-array bl] (let [input-vec input-array]
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
                                                      (if (= zero-x 0) (do (recur (next xv) (+ dur-x sum))) sum)) sum)))


(defn adjust-duration [input-vector input-original] (let [length   (count input-vector)
                                                         full-durs input-vector
                                                         input-vector (into [] (map * input-vector input-original))
                                                         idxs (vec (range length))]
                                                     (loop [xv (seq idxs)
                                                            result []]
                                                         (if xv
                                                          (let [xidx      (first xv)
                                                                nidx      (if (< (+ 1 xidx) length) (+ 1 xidx) (- length 1));   (mod (+ 1 xidx) length)
                                                                opnext    (nth input-vector nidx)
                                                                op        (nth input-vector xidx)
                                                                vec-ring  (vec (subvec idxs nidx))
                                                                op      (if (and (not= 0 op) ( = 0 opnext)) (+ op (sum-zero-durs vec-ring input-vector full-durs)) op)]
                                                            (recur (next xv) (conj result op))) result))))

(defn generate-durations [input] (let [mod-input (vec (map trigger-dur (vec (flatten input))))
                                                 durs  (traverse-vector input)
                                                 durs  (into [] (flatten durs))
                                                 durs  (adjust-duration durs (vec (flatten mod-input)))  ]
                                             {:dur durs :val (flatten input) :mod-input (vec mod-input) }))

(defn split-to-sizes [input sizes] (let [s input]
                                     (apply concat (reduce
                                                    (fn [[s xs] len]
                                                      [(subvec s len)
                                                       (conj xs (subvec s 0 len))])
                                                    [s []]
                                                    sizes))))


                                        ; The creation of buffers is slow, this function may need to be parallelised at some point in some way.
                                        ; A more feature rich input parser is needed
                                        ;Better separation of the trigger timing and trigger value functions
(defn generate-buffer-vector [field  new-buf-data] (let [new-buf-data       (map clojure.edn/read-string new-buf-data)]
                                                     (loop [xv new-buf-data
                                                            result []]
                                                      (if xv
                                                        (let [x-item      (first xv)
                                                              x-out  (generate-durations x-item)
                                                              x-item  (field x-out)
                                                              size    (count x-item)
                                                              x-item-base-dur  (/ 1 size)
                                                              x-item-leading-zeros (count (filter #{0} (first (partition-by identity x-item))))
                                                              x-item-lead-dur      (* x-item-base-dur x-item-leading-zeros)
                                                              x-item (remove zero? x-item)
                                                              x-item (vec (concat [x-item-lead-dur] x-item)) ; A silent trigger at the beginning of each pattern
                                                              x-size (count x-item)
                                                              x-buf  (buffer x-size)
                                                              _      (buffer-write-relay! x-buf (vec x-item))]
                                                          (recur (next xv) (conj result x-buf)))result))))

                                        ; fields, :dur, :val


(defn conditional-remove-zero [cond inputvec] (let [size      (count inputvec)
                                                    idxs      (range size)]
                                                (loop [xv     (seq idxs)
                                                       result []]
                                                  (if xv
                                                    (let [idx     (first xv)
                                                          cond-i  (nth cond idx )
                                                          value-i (nth inputvec idx)
                                                          ]
                                                      (if (= cond-i false) (do (recur (next xv) (conj result value-i))) (do (recur (next xv) result )) )) result))))


(defn dur-and-val-zero [durs vals] (map (fn [x y] (= x y 0)) durs vals))

(defn generate-pattern-vector [new-buf-data] (let [new-buf-data        (map clojure.edn/read-string new-buf-data)
                                                   new-durs-and-vals   (map generate-durations new-buf-data)
                                                   durs                (map :dur new-durs-and-vals)
                                                   vals                (map :val new-durs-and-vals)
                                                   mod-beat            (map :mod-input new-durs-and-vals)
                                                   base-sizes          (map count new-buf-data)
                                                   base-durations      (map (fn [x] (/ 1 x) ) base-sizes)
                                                   leading-zeros       (map (fn [x]  (count (filter #{0} (first (partition-by identity x))))) durs)
                                                   silent-trigger-durs (mapv  * base-durations leading-zeros)
                                                   durs-and-vals-zero  (mapv (fn [x y] (vec (dur-and-val-zero x y))) durs vals)
                                                   durs                (mapv (fn [x y] (conditional-remove-zero x y)) durs-and-vals-zero durs)
                                                   vals                (map (fn [x y] (conditional-remove-zero x y)) durs-and-vals-zero vals)
                                                   durs                (mapv (fn [x y] (concat [x] y)) silent-trigger-durs durs)
                                                   vals                (mapv (fn [x] ; Note, this map is a bit too much, needs simplification
                                                                               (let [size               (count vals)
                                                                                     cur-idx            x
                                                                                     prev-idx           (mod (- x 1) size)
                                                                                     cur-vec            (nth vals cur-idx)
                                                                                     prev-vec           (nth vals prev-idx)
                                                                                     prev-vec-last-item (nth prev-vec (mod -1 (count prev-vec)))
                                                                                     ]
                                                                                 (concat [0] cur-vec))) (range (count vals)))]
                                               {:dur durs :val vals}

                                               ))

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
                           triggers
                           synth-name]
  synth-control
  (kill-synth [this] (kill (. this play-synth)))
  (kill-trg   [this] (group-free (. this group)))
  (swap-synth [this synth-name] (println "not implemented"))
  (ctl-synth [this var value] (ctl (. this play-synth) var value)))


(defprotocol trigger-control
  (kill-trg-group [this])
  (get-or-create-pattern-buf [this new-size])
  (get-or-create-pattern-value-buf [this new-size]))

                                        ; TODO: Implement buffer management
                                        ; -buffer reuse
                                        ; -Freeing unneeded buffers (note: buffer-free does not seem to work, at least not in a similar way as free-bus )
                                        ; -Freeing control buses
(defrecord triggerContainer [control-key
                             control-val-key
                             group
                             play-synth
                             trigger-bus
                             trigger-value-bus
                             trigger-synth
                             pattern-vector
                             pattern-value-vector
                             pattern-buf
                             pattern-value-buf]
  trigger-control
  (kill-trg-group [this] (do (group-free (. this group))
                             (free-bus trigger-bus)
                             (free-bus trigger-value-bus)
                             (doseq [x pattern-vector] (store-buffer x))
                             (doseq [x pattern-value-vector] (store-buffer x))
                             (store-buffer pattern-buf)
                             (store-buffer pattern-value-buf)))
  (get-or-create-pattern-buf [this new-size] (let [old-size (count (. this pattern-vector))]
                                               (if (= old-size new-size) (. this pattern-buf) (do (store-buffer (. this pattern-buf))  (retrieve-buffer new-size)) )))
  (get-or-create-pattern-value-buf [this new-size] (let [old-size (count (. this pattern-value-vector))]
                                                     (if (= old-size new-size) (. this pattern-value-buf) (do (store-buffer (. this pattern-value-buf ))  (retrieve-buffer new-size)) ))))

(defn create-synth-config [pattern-name synth-name] (let [out-bus      0
                                                          synth-group  (group pattern-name :after main-g)
                                                          play-synth   (synth-name  [:tail synth-group] )
                                                          triggers     {}]
                                                      (synthContainer. pattern-name synth-group out-bus play-synth triggers synth-name)))


(defn create-trigger [control-key
                      control-val-key
                      synth-name
                      pattern-group
                      pattern-vector
                      pattern-value-vector]
  (let [trig-bus             (control-bus 1)
        trig-val-bus         (control-bus 1)
        buf-size             (count pattern-vector)
        dur-buffers          (vec (mapv (fn [x] (retrieve-buffer (count x))) pattern-vector))
        val-buffers          (vec (mapv (fn [x] (retrieve-buffer (count x))) pattern-value-vector))
        _                    (vec (mapv (fn [x y] (buffer-write-relay! x y)) dur-buffers pattern-vector ))
        _                    (vec (mapv (fn [x y] (buffer-write-relay! x y)) val-buffers pattern-value-vector))
        pattern-id-buf       (retrieve-buffer buf-size)
        pattern-value-id-buf (retrieve-buffer buf-size)
        _                    (buffer-write! pattern-id-buf       (vec (map (fn [x] (buffer-id x)) dur-buffers)))
        _                    (buffer-write! pattern-value-id-buf (vec (map (fn [x] (buffer-id x)) val-buffers)))
        trig-group           (group (str control-key) :tail pattern-group)
        trig-synth           (trigger-generator [:tail trig-group]
                                                base-trigger-bus
                                                base-trigger-count-bus
                                                pattern-id-buf
                                                pattern-value-id-buf
                                                trig-bus
                                                trig-val-bus)]
    (ctl synth-name  control-key trig-bus control-val-key  trig-val-bus  )
    (triggerContainer. control-key control-val-key trig-group synth-name trig-bus
                       trig-val-bus  trig-synth  dur-buffers val-buffers pattern-id-buf pattern-value-id-buf)))


(defn reuse-or-create-buffer [new-buf-vec] (let  [new-size    (count new-buf-vec)]
                                             (retrieve-buffer new-size)))

; TODO: reuse buffers if possible
(defn update-trigger [trigger
                      pattern-vector
                      pattern-value-vector]
  (let [buf-size             (count pattern-vector)
        old-dur-buffers      (vec (map (fn [x] (store-buffer x)) (vec (:pattern-vector trigger))))
        old-var-buffers      (vec (map (fn [x] (store-buffer x)) (vec (:pattern-value-vector trigger))))
        dur-buffers          (vec (map (fn [x] (reuse-or-create-buffer x)) pattern-vector))
        val-buffers          (vec (map (fn [x] (reuse-or-create-buffer x)) pattern-value-vector))
        _                    (vec (mapv (fn [x y] (buffer-write-relay! x y)) dur-buffers pattern-vector ))
        _                    (vec (mapv (fn [x y] (buffer-write-relay! x y)) val-buffers pattern-value-vector))
        pattern-id-buf       (get-or-create-pattern-buf trigger buf-size)
        pattern-value-id-buf (get-or-create-pattern-value-buf trigger buf-size)
        _                    (buffer-write! pattern-id-buf       (vec (map (fn [x] (buffer-id x)) dur-buffers)))
        _                    (buffer-write! pattern-value-id-buf (vec (map (fn [x] (buffer-id x)) val-buffers)))
        trigger              (assoc trigger :pattern-vector dur-buffers)
        trigger              (assoc trigger  :pattern-value-vector val-buffers)
        trigger              (assoc trigger :pattern-buf pattern-id-buf)
        trigger              (assoc trigger  :pattern-value-buf pattern-value-id-buf)
        trig-synth           (:trigger-synth trigger)]
    (ctl trig-synth :base-pattern-buffer-in pattern-id-buf :base-pattern-value-buffer-in pattern-value-id-buf)
    trigger))


(defn t [synth-container control-pair] (let [control-key       (first control-pair)
                                             control-val-key   (keyword (str (name control-key) "-val"))
                                             control-pattern   (last control-pair)
                                             pattern-vectors   (generate-pattern-vector control-pattern)
                                             trig-pattern      (:dur pattern-vectors)
                                             val-pattern       (:val pattern-vectors)
                                             pattern-group     (:group synth-container)
                                             triggers          (:triggers synth-container)
                                             play-synth        (:play-synth synth-container)
                                             trigger-status    (control-key triggers)
                                             trigger           (if (some? trigger-status) (update-trigger trigger-status trig-pattern val-pattern)
                                                                   (create-trigger control-key
                                                                                   control-val-key
                                                                                   play-synth
                                                                                   pattern-group
                                                                                   trig-pattern
                                                                                   val-pattern
                                                                                   )) ]
                                         trigger))




                                        ;input as hashmap {:pn :sn ...:controls...}

(defn trg ([input]
           (let [pattern-name          (:pn input)
                 pattern-name-key      (keyword pattern-name)
                 synth-name            (:sn input)
                 original-input        input
                 valid-keys            (concat [:pn :sn]  (vec (synth-args synth-name)))
                 input                 (select-keys input (vec valid-keys)) ; Make input valid, meaning remove control keys that are not present in the synth args
                 input                 (dissoc input :sn)
                 input-controls-only   (dissoc input :pn)
                 initial-controls-only input-controls-only
                 input-check           (some? (not-empty input-controls-only))
                 synth-container        (pattern-name-key @synthConfig)]
             (if  (= nil synth-container)
               (do (println "Synth created") (swap! synthConfig assoc pattern-name-key (create-synth-config pattern-name  synth-name)))
               (do (println "Synth exists")))
             (do  (let [synth-container                              (pattern-name-key @synthConfig)
                        triggers                                     (:triggers synth-container)
                        running-trigger-keys                         (keys triggers)
                        input-trigger-keys                           (keys initial-controls-only)
                        triggers-running-but-not-renewd              (first (diff running-trigger-keys input-trigger-keys))
                        _                                            (doseq [x triggers-running-but-not-renewd] (if (some? x) (kill-trg-group (x triggers))))
                        triggers                                     (apply dissoc triggers triggers-running-but-not-renewd)
                        synth-container                              (assoc synth-container :triggers triggers)]
                    (swap! synthConfig assoc pattern-name-key synth-container)))
             (swap! synthConfig assoc pattern-name-key
                    (assoc (pattern-name-key @synthConfig) :triggers
                           (zipmap (keys input-controls-only)   (map (partial t (pattern-name-key @synthConfig)) input-controls-only)))) pattern-name)))



(defn stop-pattern [pattern-name] (let [pattern-name-key      (keyword pattern-name)
                                    pattern-status        (pattern-name-key @synthConfig)]
                                (println pattern-status)
                                (if (some? pattern-status) (do (kill-trg pattern-status)
                                                               (swap! synthConfig dissoc pattern-name-key) ) )))



(start-trigger)
