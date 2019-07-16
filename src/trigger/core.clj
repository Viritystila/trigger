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

(def port 57110)

(defn boot-ext [] (if (server-connected?) nil (boot-external-server port {:max-buffers 262144 :max-control-bus 8096}) ))

(defn main
  []

  (boot-ext)
  ;(require '[trigger.trigger :refer :all])
  (start-trigger)
  )

(main)
