(ns #^{:author "Mikael Reponen"}
  trigger.samples
  (:use [overtone.core]
        [clojure.java.io]
        [overtone.helpers.file])
  (:import java.io.File)
  (:import java.io.FileNotFoundException))

                                        ;Sample handling
                                        ; SuperDirt sample loading adapted from https://github.com/repl-electric/cassiopeia/blob/master/src/cassiopeia/dirt.clj


(def superdirt-path (resolve-tilde-path "~/.local/share/SuperCollider/downloaded-quarks/Dirt-Samples/"))




(defonce samplePool (atom {}))
(defonce superDirtSampleNames (atom []))
(defonce superDirtSampleCount (atom []))
                                        ;Sample functions

(defn add-sample [name buf] (let [sample-data  {:id (buffer-id buf) :buf buf}
                                  sp           @samplePool
                                  sample-key   (keyword name)
                                  key-exist    (contains? sp sample-key)
                                  sp           (assoc sp (keyword name) sample-data)]
                              (reset! samplePool sp)) nil)

(defn add-samples [name buf-seq]
  (let [sz    (count buf-seq)
        nr    (range sz)
        names (map (fn [x] (clojure.string/join [name (str x)])) nr)
        names (into [] names)]
    (doseq [x nr] (add-sample (nth names x) (nth buf-seq x)))
    nil))



(defn get-sample-id [name]  (:id (name @samplePool)))

(defn list-samples [] (println (keys  (into (sorted-map) @samplePool))))

                                        ;SuperDirt samples
(defn walk-dirs [path]
  (let [dir       (file path)
        children  (.listFiles dir)
        subdirs   (filter #(.isDirectory %) children)
        subdirs   (filter #(not (.isHidden %)) subdirs)
        files     (filter #(.isFile %) children)]
    (reset! superDirtSampleNames (concat @superDirtSampleNames
                                       (mapv (fn [x] (last (clojure.string/split (.getPath x) #"/"))) subdirs) ))
    (reset! superDirtSampleCount (concat @superDirtSampleCount
                                         (mapv (fn [x]
                                                 (let [nofiles (count (.listFiles x))
                                                       norange (range 1 (+ 1 nofiles))]
                                                   (vec norange)) ) subdirs)))
))

(defn walk [^File dir]
  (let [children  (.listFiles dir)
        subdirs   (filter #(.isDirectory %) children)
        files     (filter #(.isFile %) children)]
    (concat files (mapcat walk subdirs))))


(defn find-SD-sample-names []
  (reset! superDirtSampleNames [])
  (reset! superDirtSampleCount [])
  (walk-dirs superdirt-path) nil)

(defn dirt
  "Fetches and caches locally dirt samples. All dirt samples are refered to by a containing folder `sample-name` and an int `n` which specifies which file to play. Only wav + aiff are supported by Overtone so ignore anything else.
   Example:
    (sample-player (dirt :amp 0))"
  ([sample-name] (dirt sample-name 1))
  ([sample-name n]
     (if-let [sample (@samplePool (str (name sample-name)  n))]
       sample
       (let [sample-name (name sample-name)
             samples (->> (walk (file (str superdirt-path sample-name "/")))
                          (filter #(re-find #"(?i)wav|aiff" (or (file-extension %1) ""))))
             n (if (>= n (count samples)) 0 n)
             sample-file (nth samples n)]
         (when sample-file
           (add-sample (str sample-name  n) (load-sample sample-file) )
           (@samplePool (str sample-name n)))))))


(defn load-all-SuperDirt-samples []
  (future
    (find-SD-sample-names)
    (let [sample-count    (count @superDirtSampleNames)]
      (doseq [sample-name-idx (range sample-count)]
        (doseq [sample-no-id (nth @superDirtSampleCount sample-name-idx)]
          (dirt (nth @superDirtSampleNames sample-name-idx) sample-no-id) ))))
  )
