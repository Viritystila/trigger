(ns #^{:author "Mikael Reponen"}
  trigger.misc
  (:use [overtone.core]))

(defn trigger-logo
  []
  (str "
    ▒            ░██
    █
  █████ ████████░███   ███ ██░█  ██ ░██░░ ▒██ ██  ███▒████
   ██    ░██      ██   ██  ▓██  ███  ██░ ███▒▒▒██  ███
   ██░ █ ░██      ██   ▓█▓░░     ██▒░    ▓██    █  ███
   ▓███ ██████  ▒████░ ███████░ ████████   █████  █████
                      ██░   ░█░ ██    ██
                                           "))
