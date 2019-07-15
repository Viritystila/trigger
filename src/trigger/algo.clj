(ns #^{:author "Mikael Reponen"}
  trigger.algo
  (:use [overtone.core])
  (:require [markov-chains.core]
            [clojure.set :as set]
            [overtone.algo.euclidean-rhythm :refer :all]))




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

;trg-related
(defn urn [n]
  "Unique random numbers, e.g. (1 3 2  5)"
  (let [a-set (set (take n (repeatedly #(rand-int n))))]
    (concat a-set (set/difference (set (take n (range)))
                                  a-set))))


(defn fst ([factor input] (vec (repeat factor (seq input ) )))
  ([input] (vec (repeat 2 (seq input)))))

(defn slw ([factor input] (let [input-size (count input)
                                factor     (int (/  input-size factor))
                                ] (map vec (partition factor input))))
  ([input] (let  [input-size (count input)
                                factor     (int (/  input-size 2))
                                ] (map vec (partition factor input)))))

(defn rev [coll] (vec (reverse coll)))

(defn evr [n f & coll] (let [isfn  (fn? f)
                             coll_length (count coll)
                             coll        (if (= 1 coll_length) (apply concat coll) coll )]
                         (if isfn
                           (map-indexed #(if (zero? (mod (inc %1) n))  (f %2) %2) coll)
                            (map-indexed #(if (zero? (mod (inc %1) n)) f %2) coll) )))

(defn rtm [pulses steps & args]  (mapv (fn [x] (if (zero? x) "-" x))  (vec (euclidean-rhythm (max 1 (func-val pulses args)) steps))))

(defn nts [& notes] (mapv (fn [x] (if (keyword? x) (note x) x) ) notes))

(defn chr [root chord-name] (vec (chord root chord-name)) )

(defn chd ([degree root mode] (vec (chord-degree degree root mode)))
  ([degree root mode num-notes] (vec (chord-degree degree root mode num-notes))))


(defn mhz [& notes] (mapv (fn [x] (if (keyword? x) (midi->hz (note x)) x) ) notes))

(defn rep ([n input]  (let [isfn   (fn? input)]
                        (if isfn (repeatedly n #(input))  (repeat n input))))
  ([n fnc & args] (repeatedly n #(apply fnc args) ) ))

(defn sfl [coll] (shuffle coll))

(defn rpl ([n input & coll]  (let [coll_length (count coll)
                                   isseq       (seq? (first coll))
                                   coll        (if  isseq (apply concat coll)
                                                   (apply concat coll) )]
                               (if isseq (seq (assoc (vec coll) n input ))
                                   (vec (assoc (vec coll) n input))))))

(defn sir [n range center period] (map (fn [x] (sinr x range center period)) (clojure.core/range n)))


(defn cor [n range center period] (map (fn [x] (cosr x range center period)) (clojure.core/range n)))


(defn tar [n range center period] (map (fn [x] (tanr x range center period)) (clojure.core/range n)))

(defn rot [n coll] (rotate n coll))

(defn fll [size coll] (fill size coll))

(defn del [beat del_val coll] (let [coll_length (count coll)
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
