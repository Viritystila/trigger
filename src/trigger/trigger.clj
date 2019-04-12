(ns #^{:author "Mikael Reponen"}
  trigger.trigger
  (:use [overtone.live])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))
  


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
   
   
   
   
   
   
  ;trigger function testing
  
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
 
(defsynth tstsin [trig-in 0 f 200] (let [trg (in:kr trig-in)
                                   env (env-gen (perc 0.01 0.01 1 0) :gate trg)
                                   src (* env (sin-osc f))]
                               (out 0 src)))
  
(defn trg [pattern-name synth-name pattern] (let [pattern-name-symbol  (symbol pattern-name)
                                                 resolved-pattern     (resolve pattern-name-symbol)
                                                 type-pattern-name    (if (some? resolved-pattern) (type (var-get resolved-pattern)) nil)
                                                 synth-node-status    (if  (= overtone.sc.node.SynthNode type-pattern-name )
                                                                        (node-live? (var-get resolved-pattern))
                                                                        nil)      ]
                                             (if  synth-node-status
                                               (do (println "Synth exits"))
                                               (do (println "Synth created") (intern *ns* (symbol pattern-name) (synth-name tstbus )) )  )))   
  
  
  ;pattern generation functions
(defn triggerDur [dur] (if (= dur 0) 0 1) )

 (defn traverseVector ([input-array] (let [input-vec input-array
                                       ;_ (println input-vec)
                                           result []]
                                       (if true ;(vector? input-vec)
                                         (loop [xv (seq input-vec)
                                                result []]
                                           (if xv
                                             (let [;_ (println xv)
                                                   length (count input-vec)
                                                   x (first xv)]
                                               (if (vector? x) (recur (next xv) (conj result (traverseVector x length)))
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
                               (if (vector? x) (recur (next xv) (conj result (traverseVector x (* bl length))))
                                   (recur (next xv) (conj result (/ 1 length bl))))) result))))))


 (defn sumZeroDurs [idxs input-vector full-durs] (loop [xv (seq idxs)
                                                        sum 0]
                                                   (if xv
                                                     (let [x       (first xv)
                                                           zero-x  (nth input-vector x )
                                                           dur-x   (nth full-durs x)]
                                                       (println zero-x)
                                                       (println dur-x)
                                                       (if (= zero-x 0) (do (recur (next xv) (+ dur-x sum))) sum)) sum)))


(defn adjustDuration [input-vector input-original] (let [length   (count input-vector)
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
                                                                 op      (if (and (not= 0 op) ( = 0 opnext)) (+ op (sumZeroDurs vec-ring input-vector full-durs)) op)]
                                                             (recur (next xv) (conj result op))) result))))

  (defn generateDurations [input] (let [mod-input (vec (map triggerDur (vec (flatten input))))
                                        durs  (traverseVector input)
                                        durs  (into [] (flatten durs))
                                        durs  (adjustDuration durs (vec (flatten mod-input)))]
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
  
  
(defn start []     
    (defonce base-trigger-bus (control-bus 1))
    (defonce base-trigger-dur-bus (control-bus 1))
    (control-bus-set! base-trigger-dur-bus 1)
    (def base-trigger (baseTrigger base-trigger-dur-bus base-trigger-bus))
    (def base-trigger-count-bus (control-bus 1))
    (def base-trigger-count (baseTriggerCounter base-trigger-bus base-trigger-count-bus))


)
