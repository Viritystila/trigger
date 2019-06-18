(ns #^{:author "Mikael Reponen"}
  trigger.algo
  (:use [overtone.core])
  (:require [markov-chains.core]))



(def sC (atom 0))

(defn init-algo [sc-in] (def sC sc-in))

;(defn alg [buf algorithm] (println "not implemented yet"))

(defsynth ping_tst
  [note   72
   attack 0.02
   decay  0.3]
  (let [snd (sin-osc (midicps note))
        env (env-gen (perc attack decay) :action FREE)]
    (out 0 (* 0.13 env snd))))


(defn foo
  [t freq]
  (at t (ping_tst freq))
  (let [next-t (+ t 1000)
        next-f (+ freq 0)]
     (apply-by next-t #'trigger.algo/foo [next-t next-f])
    )
  nil)
