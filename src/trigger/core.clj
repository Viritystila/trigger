(ns #^{:author "Mikael Reponen"}
  trigger.core
  (:use [overtone.core]
        [clojure.data])
  (:require
   [trigger.trigger :refer :all]
   [trigger.synths :refer :all]
   [trigger.misc :refer :all]
   [trigger.algo :refer :all]
   [clojure.tools.namespace.repl :refer [refresh]]))

(defn main
  []
  (println "start")
  )
