(ns #^{:author "Mikael Reponen"}
  trigger.speech
  (:use [overtone.core])
  (:require [markov-chains.core]
            [clojure.set :as set]
            [overtone.algo.euclidean-rhythm :refer :all]
            [clojure.math.numeric-tower :as math])
  (:import [marytts LocalMaryInterface]
           [marytts.exceptions SynthesisException]
           [marytts.util.data.audio MaryAudioUtils]))

                                                   ;Text-to-speech

           
(defsynth smpl [buf 0] (out 0 (play-buf 1 buf)))

           
(def marytts (new LocalMaryInterface))

(defn word-to-buffer [word]
  (let [audio (.generateAudio marytts word)
        audio-array  (marytts.util.data.audio.MaryAudioUtils/getSamplesAsDoubleArray audio)
        audio-buffer (buffer (count audio-array))
        _            (buffer-write-relay! audio-buffer 0 audio-array)]
    audio-buffer))
