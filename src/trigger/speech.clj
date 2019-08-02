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


(defsynth smpl [buf 0] (out 0 (play-buf 1 buf :action FREE)))


(defsynth smpl2 [buf 0] (let [td  (t-duty:ar  0.0002083333 0 1)
                              cntr (pulse-count:ar td)
                              bf  (demand:ar td 1 (dbufrd buf (dseries 0 1 INF) 1))
                              ;bf   (buf-rd 1 buf cntr 1)
                              ]
                          (out 0 (* 1 bf))))

(def marytts (new LocalMaryInterface))

(defn word-to-buffer [word]
  (let [audio (.generateAudio marytts word)
        audio-array  (marytts.util.data.audio.MaryAudioUtils/getSamplesAsDoubleArray audio)
        audio-buffer (buffer (count audio-array))
        _            (buffer-write-relay! audio-buffer 0 audio-array)]
    audio-buffer))

(defn say [word] (let [ab  (word-to-buffer word)
                       _   (smpl ab)
                       ;_   (println (:duration (buffer-info ab)))
                       dur (:duration (buffer-info ab))
                       _   (Thread/sleep (* dur 1000))
                       _  (buffer-free ab)
                       ]) nil)


(defn sayb [buffer] (let [;ab  (word-to-buffer word)
                       _   (smpl buffer)
                       ;_   (println (:duration (buffer-info ab)))
                       dur (:duration (buffer-info buffer))
                       _   (Thread/sleep (* dur 1000))
                       ;_  (buffer-free ab)
                       ]) nil)


                                        ;Markov tests
; Taken from https://github.com/rm-hull/markov-chains
(defn three-men-in-a-boat []
  (->
    (slurp "src/trigger/308.txt")
    (clojure.string/split #"\s+")
    (markov-chains.core/collate 2)))

(defn tmoab []
  (->>
   (markov-chains.core/generate (three-men-in-a-boat))
   (take 60)
   (clojure.string/join " ")))

;(doseq [x (range 1110 1169)] (trigger.speech/sayb x))
