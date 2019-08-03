(ns #^{:author "Mikael Reponen"}
  trigger.speech
  (:use [overtone.core])
  (:require [markov-chains.core]
            [clojure.set :as set]
            [overtone.algo.euclidean-rhythm :refer :all]
            [clojure.math.numeric-tower :as math]
            [clojure.java.io :as io])
  (:import [marytts LocalMaryInterface]
           [marytts.exceptions SynthesisException]
           [marytts.util.data.audio MaryAudioUtils]))

                                                   ;Text-to-speech


(defsynth smpl [buf 0] (out 0 (play-buf 1 buf 1.0 0 1 :action FREE)))



(defn sm [buf] (let [pp (smpl buf)] nil) )

(def marytts (new LocalMaryInterface))

(defn string-to-buffer [word]
  (let [audio (.generateAudio marytts word)
        audio-array  (marytts.util.data.audio.MaryAudioUtils/getSamplesAsDoubleArray audio)
        audio-buffer (buffer (count audio-array))
        _            (buffer-write-relay! audio-buffer 0 audio-array)]
    audio-buffer))

(defn say [word] (let [ab  (string-to-buffer word)
                       _   (smpl ab)
                       ;_   (println (:duration (buffer-info ab)))
                       dur (:duration (buffer-info ab))
                       _   (Thread/sleep (* dur 1000))
                       _  (buffer-free ab)
                       ]) nil)


(defn split-sentence [x dm] (clojure.string/split x dm))

(defn sentence-to-buffers [sent dm]
  (let [ss (split-sentence sent dm)]
    (map (fn [x] (string-to-buffer x)) ss )))


                                        ;Markov tests
                                        ; Taken from https://github.com/rm-hull/markov-chains

(defn load-markov-source-text [path]
  (let [isfile       (.exists (io/as-file path))
        text         (if isfile (slurp path) path)
        split-text   (clojure.string/split text #"\s+")
        mkc          (markov-chains.core/collate split-text 2)] mkc))

(defn generate-markov-text [path noitems]
  (let [pm             (load-markov-source-text path)
        words          (markov-chains.core/generate pm)
        selected-words (take noitems words)
        mkc-text       (clojure.string/join " " selected-words)]
    mkc-text) )

(defn tmoab [] (generate-markov-text  "src/trigger/308.txt" 60) )

;(doseq [x (range 1110 1169)] (trigger.speech/sayb x))
