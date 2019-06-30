(ns #^{:author "Mikael Reponen"}
  trigger.algo
  (:use [overtone.core])
  (:require [markov-chains.core]))



(def sC (atom 0))

(defn init-algo [sc-in] (def sC sc-in))

(def first-order-prob-matrix {
  [:en] { :en 0.1   :qn 0.06  :hn 0.3 }
  [:qn] { :en 0.925 :qn 0.05  :hn 0.07 }
    [:hn] { :en 0.7   :qn 0.03  :hn 0.9 }})


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
