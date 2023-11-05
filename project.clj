(defproject org.viritystila/trigger "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://github.com/Viritystila/cutter"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :repositories [["jcenter" "https://jcenter.bintray.com/"]]
  :dependencies [[org.clojure/clojure "1.10.0"]
                [org.clojure/tools.namespace "0.2.11"]
                [org.clojure/core.async "0.4.490"]
                [overtone-javacpp            "0.11.0"]
                [org.bytedeco/SuperCollider  "13.3.0-1.5.9"]
                [org.bytedeco/SuperCollider-platform "13.3.0-1.5.9"]
                [rm-hull/markov-chains "0.1.1"]
                [org.clojure/math.numeric-tower "0.0.4"]
                [de.dfki.mary/marytts "5.2" :extension "pom"]
                [de.dfki.mary/marytts-runtime "5.2"]
                [de.dfki.mary/voice-cmu-slt-hsmm "5.2"]
                [de.dfki.mary/voice-enst-camille-hsmm "5.2"]]
  :repl-options {:init-ns trigger.trigger :timeout 9220000}
  :native-path "native"
  :main  ^{:skip-aot true}  trigger.trigger
  :jvm-opts ^:replace [])
