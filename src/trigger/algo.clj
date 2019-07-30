(ns #^{:author "Mikael Reponen"}
  trigger.algo
  (:use [overtone.core])
  (:require [markov-chains.core]
            [clojure.set :as set]
            [overtone.algo.euclidean-rhythm :refer :all]
            [clojure.math.numeric-tower :as math]))




(defn unique-random-numbers [n]
  (let [a-set (set (take n (repeatedly #(rand-int n))))]
    (concat a-set (set/difference (set (take n (range)))
                                  a-set))))


;Markov chain example
(def note-fobm {
                [:A2]  { :A2 0.1  :C#2 0.06  :E2 0.3 }
                [:C#2] { :A2 0.925 :C#2 0.05 :E2 0.07 }
                [:E2] { :A2 0.7  :C#2 0.03  :E2 0.9 }})



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
                                  dur  (/ 1 rnmd)]  (vec (take 1 (markov-chains.core/generate @fobm_args)))
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
;general
(defn func-val [input & args] (let [isfn  (fn? input)
                                    coll_length (count args)
                                    args        (if (= 1 coll_length) (apply concat args) args )]
                                (if isfn (apply input args) input )
                                ) )

(defn cnc [& input] (let [isseq (seq input)
                          ;input  (vec (reverse input))
                          input (apply conj  input)]
                    (if isseq (conj input)
                        (concat input))))


(defn piv [input] (let []
                                   (loop [xv     (seq input)
                                          result []]
                                     (if xv
                                       (let [fst     (first xv) ]
                                         (if (vector? fst) (recur (next xv) (conj result (vec (piv fst))))
                                             (if (seq? fst) (recur (next xv) (apply conj result  (vec (piv fst))))
                                                 (recur (next xv) (conj result fst))))) result ))))

;trg-related
(defn urn [n]
  "Unique random numbers, e.g. (1 3 2  5)"
  (let [a-set (set (take n (repeatedly #(rand-int n))))]
    (concat a-set (set/difference (set (take n (range)))
                                  a-set))))


(defn fst ([factor input] (piv (vec (repeat factor (seq input ) ))))
  ([input] (piv (vec (repeat 2 (seq input))))))

(defn slw ([factor input] (let [input      (piv input)
                                input-size (count input)
                                factor     (int (/  input-size factor))]
                            (seq (piv (map vec (partition factor input))))))
  ([input] (let  [input   (piv input)
                  input-size (count input)
                  factor     (int (/  input-size 2))]
             (seq (piv (map vec (partition factor input)))))))

(defn rev [coll]  (piv (reverse (piv coll))))

(defn evr [n f & coll] (let [isfn        (fn? f)
                             coll        (piv coll)
                             coll_length (count coll)
                             ]
                         (if isfn
                           (map-indexed #(if (zero? (mod (inc %1) n))  (f %2) %2) coll)
                            (map-indexed #(if (zero? (mod (inc %1) n)) f %2) coll) )))

(defn rtm [pulses steps & args]  (map (fn [x] (if (zero? x) "-" x))  (vec (euclidean-rhythm (max 1 (func-val pulses args)) steps))))

(defn nts [& notes] (map (fn [x] (if (keyword? x) (note x) x) ) notes))

(defn chr [root chord-name] (seq (chord root chord-name)) )

(defn chd ([degree root mode] (seq (chord-degree degree root mode)))
  ([degree root mode num-notes] (seq (chord-degree degree root mode num-notes))))


(defn mhz [& notes] (map (fn [x] (if (keyword? x) (midi->hz (note x)) x) ) notes))

(defn rep ([n input]  (let [isfn   (fn? input)]
                        (if isfn (seq (piv (repeatedly n #(input))))  (seq (piv (repeat n  input))))))
  ([n fnc & args] (seq (piv (repeatedly n #(apply fnc args) ))) ))

(defn sfl [& coll] (let [isseq (seq? (first coll))
                         isseq (if (= 1 (count coll )) true false )]
                     (if isseq (vec (shuffle (first coll)))
                         (seq (shuffle coll)))))

(defn rpl ([n input & coll]  (let [coll_length (count coll)
                                   isseq       (seq? (first coll))
                                   isfn        (fn? input)
                                   coll        (if  isseq (apply concat coll)
                                                    coll )
                                   ]
                               (if isseq (seq (assoc (vec coll) n input ))
                                   (seq (assoc (vec coll) n input))))))

(defn sir [n range center period] (map (fn [x] (sinr x range center period)) (clojure.core/range n)))


(defn cor [n range center period] (map (fn [x] (cosr x range center period)) (clojure.core/range n)))


(defn tar [n range center period] (map (fn [x] (tanr x range center period)) (clojure.core/range n)))

(defn sqr [n x1 x2 high low] (map (fn [x] (if (and (>= x x1) (< x x2)) high low )) (range n) ))

(defn rot [n coll] (rotate n (piv coll)))

(defn fll [size coll] (fill size coll))

(defn del [beat del_val coll] (let [coll        (piv coll)
                                    coll_length (count coll)
                                    beat        (mod beat coll_length)
                                    coll_val    (nth coll beat)
                                    is_coll_val_vec (vector? coll_val)
                                    delay_count  (+ 1 del_val)
                                    delay_vector (vec (repeat delay_count "-"))
                                    delay_vector (if is_coll_val_vec
                                                   (assoc delay_vector del_val (seq coll_val))
                                                   (assoc delay_vector del_val coll_val) )]
                                 (assoc coll beat delay_vector)))


(defn adv [coll beat del_val] (let [
                                    ;coll_length    (count coll)
                                    ;beat           (mod beat coll_length)
                                    ;coll_val       (nth coll beat)
                                    ;prev_coll_val  (nth coll (mod (- beat 1) coll_length))
                                    ;coll           (assoc coll beat "-")
                                   ]))

(defn acc ([coll] (let [coll    (piv coll)
                        s       (count coll)
                        rcollr  (range s)
                        rcoll   (mapv (fn [x]  (repeat x "-")) rcollr )
                        rcoll   (reverse rcoll)
                        coll    (partition 1 coll)
                        pcoll   (vec (interleave coll rcoll))
                        pcoll   (apply concat pcoll)] pcoll)))


(defn dec ([coll] (let [coll    (piv coll)
                        s       (count coll)
                        rcollr  (range s)
                        rcoll   (mapv (fn [x]  (repeat x "-")) rcollr )
                        coll    (partition 1 coll)
                        pcoll   (vec (interleave rcoll coll))
                        pcoll   (apply concat pcoll)] pcoll)))
