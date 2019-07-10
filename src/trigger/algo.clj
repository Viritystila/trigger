(ns #^{:author "Mikael Reponen"}
  trigger.algo
  (:use [overtone.core])
  (:require [markov-chains.core]
            [overtone.algo.euclidean-rhythm :refer :all]))



(def sC (atom 0))

(defn init-algo [sc-in] (def sC sc-in))

(def note-fobm {
                [:A3]  { :A3 0.1  :C#3 0.06  :E3 0.3 }
                [:C#3] { :A3 0.925 :C#3 0.05 :E3 0.07 }
                [:E3] { :A3 0.7  :C#3 0.03  :E3 0.9 }})



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
                                  dur  (/ 1 rnmd)
                                  dur]  (vec (take 1 (markov-chains.core/generate @fobm_args)))
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


;Functions

(defn fst [factor input] (vec (repeat factor (seq input ) )))

(defn slw [factor input] (map vec (partition factor input)))

(defn rev [coll] (vec (reverse coll)))

(defn evr [n f & coll] (let [coll_length (count coll)
                             coll        (if (= 1 coll_length) (apply concat coll) coll )]
                           (map-indexed #(if (zero? (mod (inc %1) n)) (vec (f %2)) %2) coll)))

(defn rtm [pulses steps]  (mapv (fn [x] (if (zero? x) "-" x))  (vec (euclidean-rhythm pulses steps))))

(defn nts [& notes] (mapv (fn [x] (if (keyword? x) (note x) x) ) notes))

(defn mhz [& notes] (mapv (fn [x] (if (keyword? x) (midi->hz (note x)) x) ) notes))

(defn rep [n input] (repeat n input))
