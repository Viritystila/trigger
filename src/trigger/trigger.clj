(ns #^{:author "Mikael Reponen"}
  trigger.trigger
  (:use [overtone.core]
        [clojure.data])
  (:require
   [trigger.synths :refer :all]
   [trigger.misc :refer :all]
   [trigger.algo :refer :all]
   [trigger.speech :refer :all]
   [trigger.samples :refer :all]
   [trigger.trg_fx :refer :all]
   [clojure.tools.namespace.repl :refer [refresh]]) )

                                        ;Boot Supercollider

(def port 57111)

(defn boot-ext [] (if (server-connected?) nil (boot-external-server port {:max-buffers 2621440 :max-control-bus 80960}) ))
(boot-ext)


                                        ;State atoms
(defonce synthConfig (atom {}))
(defonce algConfig (atom {}))
(defonce bufferPool (atom {}))
(def timeatom (atom 0))

(defn init_groups_dur_and_del []
                                        ;Groups
  (do
    (defonce main-g (group "main group")))

                                        ;Base duration
  (do
    (def base-dur (buffer 1))
    (buffer-write! base-dur [1]))

                                        ; Base trigger delay
  (do
    (def base-del (buffer 1))
    (buffer-write! base-del [0])))


                                        ;Synthdefs

(defsynth base-trigger-synth [out-bus 0 trigger-id 0 base-dur-in 0 base-del-in 0]
  (let [trg1  (t-duty:kr  (dbufrd base-dur-in (dseries 0 1 INF)) 0 1)
        trg   (t-delay:kr trg1  (buf-rd:kr 1 base-del-in))]
    (send-trig trg trigger-id trg)
    (out:kr out-bus trg)))


(defsynth base-trigger-counter [base-trigger-bus-in 0 base-trigger-count-bus-out 0]
  (out:kr base-trigger-count-bus-out (pulse-count:kr (in:kr base-trigger-bus-in))))

(def dbg (control-bus 1))
(def dbg2 (control-bus 1))

(defsynth trigger-generator [base-trigger-bus-in 0
                             base-counter-bus-in 0
                             base-pattern-buffer-in 0
                             base-pattern-value-buffer-in 0
                             trigger-bus-out 0
                             trigger-value-bus-out 0
                             trigger-id 0
                             trigger-val-id 0
                             base-dur-in 0]
  (let [base-trigger            (in:kr base-trigger-bus-in)
        base-counter            (in:kr base-counter-bus-in)
        pattern-buffer-id       (dbufrd base-pattern-buffer-in base-counter)
        pattern-value-buffer-id (dbufrd base-pattern-value-buffer-in base-counter)
        pattern-first-value     (demand:kr base-trigger base-trigger (dbufrd pattern-buffer-id (dseries 0 1 1) 0) )
        pattern-value-start-idx (select:kr (= 0.0 pattern-first-value) [0 1])
                                        ; Depending on if a spacer trigger exists or not in the first index of a buffer,
                                        ;this value needs to be either 1 or 0 in order to play the buffer as intended.
        trg                     (t-duty:kr (* (dbufrd base-dur-in (dseries 0 1 INF) ) (dbufrd pattern-buffer-id (dseries 0 1 INF) 0))
                                           base-trigger
                                           (dbufrd pattern-buffer-id (dseries 0 1 INF) 0))
        pattern-item-value      (demand:kr trg base-trigger (dbufrd pattern-value-buffer-id (dseries pattern-value-start-idx 1 INF)))
        pattern-trg-value       (demand:kr trg base-trigger (dbufrd pattern-buffer-id (dseries pattern-value-start-idx 1 INF)))
        cntr  (pulse-count:kr trg base-trigger)
        trg  (select:kr (= 0 cntr) [trg 0])
        ;_ (out:kr dbg pattern-value-start-idx)
        ;_ (out:kr dbg2 pattern-item-value)
        ]
    (send-trig trg trigger-id pattern-trg-value)
    (send-trig trg trigger-val-id pattern-item-value)
    (out:kr trigger-value-bus-out pattern-item-value)
    (out:kr trigger-bus-out trg)))

                                        ;Buffer pool functions
(defn store-buffer [buf]
  (let [size      (buffer-size buf)
        size-key  (keyword (str size))
        pool      @bufferPool
        pool      (update pool size-key (fnil concat []) [buf])]
    (reset! bufferPool pool)))

(defn retrieve-buffer [size]
  (let [size-key      (keyword (str size))
        pool          @bufferPool
        buffers-left  (and (contains? pool size-key) (< 0 (count (size-key pool))))
        first-buf     (first (size-key pool))
        rest-buf      (rest  (size-key pool))
        pool          (assoc pool size-key  rest-buf )]
    (if buffers-left
      (do (reset! bufferPool pool) first-buf)
      (do (buffer size)))))

(defn generate-buffer-pool [sizes amount]
  (let [size-vectors  (range 1 sizes)
        size-keys     (map (fn [x] (keyword (str x))) size-vectors )
        buffers       (doall (pmap (fn [y]  (doall (map (fn [x] (buffer x)) (repeat  amount y)))) size-vectors))
        b_p           (zipmap size-keys buffers)]
   (reset! bufferPool b_p)) nil)
                                        ;Start

(defn start-trigger []
  (init_groups_dur_and_del)
  (def base-trigger-id (trig-id))
  (def base-trigger-bus (control-bus 1))
  (def external-trigger-bus (control-bus 1))
  (def base-trigger-dur-bus (control-bus 1))
  (control-bus-set! base-trigger-dur-bus 1)
  (buffer-write! base-dur [(/ 1 0.5625)])
  (def base-trigger (base-trigger-synth [:tail main-g] base-trigger-bus base-trigger-id base-dur base-del))
  (def base-trigger-count-bus (control-bus 1))
  (def base-trigger-count (base-trigger-counter [:tail main-g] base-trigger-bus base-trigger-count-bus))
  (println "Begin generating buffer pool, please wait.")
  (generate-buffer-pool 64 64)
  (println "trigger initialized")
  (println (trigger-logo))
  )


                                        ;Default value bus generation
(defn is-in-arg [arg-key arg-val]
  (if (some? (re-find #"in-" (str arg-key))) {(keyword arg-key) arg-val} nil ))

(defn get-in-defaults [synth-name]
  (let [arg-names        (map :name (:params synth-name))
        arg-keys         (map :default (:params synth-name))
        arg-val-keys     (map (fn [x] (keyword (str (name x) "-val"))) arg-keys)
        args-names-vals  (map (fn [x y] (is-in-arg x y)) arg-names arg-keys)
        args-names-vals  (remove nil? args-names-vals)
        args-names-vals  (apply conj args-names-vals)]
    args-names-vals) )

(defn generate-default-buses  [synth-name]
  (let [anv        (get-in-defaults synth-name)
        buses-def  (vec (map (fn [x]   (control-bus 1)) (vals anv)))
        set-buses  (vec (map (fn [x y] (control-bus-set! x y)) buses-def (vals anv )))
        buses      (vec (map (fn [x y] {x y}) (keys anv) buses-def))
        buses      (apply conj buses)]
    buses))


                                        ;Pattern generation functions
(defn trigger-dur [dur]
  (if (= dur 0) 0 1) )

(defn traverse-vector ([input-array]
                       (let [input-vec input-array
                             result []]
                         (if true
                           (loop [xv (seq input-vec)
                                  result []]
                             (if xv
                               (let [ length (count input-vec)
                                     x (first xv)]
                                 (if (vector? x) (recur (next xv) (conj result (traverse-vector x length)))
                                     (recur (next xv) (conj result (/ 1 length 1))))) result)))))
  ([input-array bl] (let [input-vec input-array]
                      (if (vector? input-vec)
                        (loop [xv (seq input-vec)
                               result []]
                          (if xv
                            (let [length (count input-vec)
                                  x (first xv)]
                               (if (vector? x) (recur (next xv) (conj result (traverse-vector x (* bl length))))
                                (recur (next xv) (conj result (/ 1 length bl))))) result))))))


(defn sum-zero-durs [idxs input-vector full-durs]
  (loop [xv (seq idxs)
         sum 0]
    (if xv
      (let [x       (first xv)
            zero-x  (nth input-vector x )
            dur-x   (nth full-durs x)]
        (if (= zero-x 0) (do (recur (next xv) (+ dur-x sum))) sum)) sum)))


(defn adjust-duration [input-vector input-original]
  (let [length   (count input-vector)
        full-durs input-vector
        input-vector (into [] (map * input-vector input-original))
        idxs (vec (range length))]
    (loop [xv (seq idxs)
           result []]
      (if xv
        (let [xidx      (first xv)
              nidx      (if (< (+ 1 xidx) length) (+ 1 xidx) (- length 1))
              opnext    (nth input-vector nidx)
              op        (nth input-vector xidx)
              vec-ring  (vec (subvec idxs nidx))
              op      (if (and (not= 0 op) ( = 0 opnext)) (+ op (sum-zero-durs vec-ring input-vector full-durs)) op)]
          (recur (next xv) (conj result op))) result))))


(defn generate-durations [input input-val]
  (let [mod-input (vec (map trigger-dur (vec (flatten input))))
        durs  (traverse-vector input)
        durs  (into [] (flatten durs))
        mod-beat durs
        durs  (adjust-duration durs (vec (flatten mod-input)))]
    {:dur durs :val (flatten input-val) :mod-input mod-beat }))


(defn split-to-sizes [input sizes]
  (let [s input]
    (apply concat (reduce
                   (fn [[s xs] len]
                     [(subvec s len)
                      (conj xs (subvec s 0 len))])
                   [s []]
                   sizes))))



(defn conditional-remove-zero [cond inputvec]
  (let [size      (count inputvec)
        idxs      (range size)]
    (loop [xv     (seq idxs)
           result []]
      (if xv
        (let [idx     (first xv)
              cond-i  (nth cond idx )
              value-i (nth inputvec idx)]
          (if (= cond-i false) (do (recur (next xv) (conj result value-i))) (do (recur (next xv) result )) )) result))))


(defn dur-and-val-zero [durs vals]
  (map (fn [x y] (= x y 0)) durs vals))

(defn string-zero-to-one [str-in]
  (clojure.string/replace  str-in #"0" "1") )

(defn string-hyphen-to-zero [str-in]
  (clojure.string/replace str-in #"~" "0") )

(defn generate-pattern-vector [new-buf-data]
  (let [new-trig-data       (vec (map (fn [x] (string-zero-to-one x)) new-buf-data))
        new-trig-data       (map clojure.edn/read-string (vec (map (fn [x] (string-hyphen-to-zero x)) new-trig-data)))
        new-val-data        (map clojure.edn/read-string (vec (map (fn [x] (string-hyphen-to-zero x)) new-buf-data)))
        new-durs-and-vals   (map generate-durations new-trig-data new-val-data)
        durs                (map :dur new-durs-and-vals)
        vals                (map :val new-durs-and-vals)
        mod-beat            (map :mod-input new-durs-and-vals)
        base-sizes          (map count new-trig-data)
        base-durations      (map (fn [x] (/ 1 x) ) base-sizes)
        leading-zeros       (map (fn [x]  (count (filter #{0} (first (partition-by identity x))))) durs)
        silent-trigger-durs (mapv (fn [x y] (into [] (subvec x 0 y))) mod-beat leading-zeros )
        silent-trigger-durs (mapv (fn [x] (reduce + x)) silent-trigger-durs)

        durs-and-vals-zero  (mapv (fn [x y] (vec (dur-and-val-zero x y))) durs vals)
        durs                (mapv (fn [x y] (conditional-remove-zero x y)) durs-and-vals-zero durs)
        vals                (map (fn [x y] (conditional-remove-zero x y)) durs-and-vals-zero vals)
        durs                (mapv (fn [x y] (concat [x] y)) silent-trigger-durs durs)
        vals                (mapv (fn [x]
                                    (let [cur-idx            x
                                          cur-vec            (nth vals cur-idx)]
                                      (concat [0] cur-vec))) (range (count vals)))]
    {:dur durs :val vals}))

                                  ;pattern timing adjustments
(defn set-pattern-duration [dur]
  (buffer-write! base-dur [dur])nil)

(defn set-pattern-delay [delay]
  (buffer-write! base-del [delay]) nil)



                                        ;Synth trigger generation

(defprotocol synth-control
  (kill-synth [this])
  (kill-trg   [this])
  (ctl-synth [this var value])
  (free-default-buses [this])
  (free-out-bus [this])
  (free-secondary-out-bus [this])
  (apply-default-buses [this])
  (apply-default-bus [this db])
  (apply-control-out-bus [this])
  (apply-out-bus [this])
  (apply-secondary-out-bus [this])
  (free-control-out-bus [this])
  (set-out-channel [this channel]))

(defrecord synthContainer [is-inst
                           pattern-name
                           group
                           out-bus
                           out-bus-secondary
                           play-synth
                           triggers
                           synth-name
                           default-buses
                           control-out-bus
                           out-mixer
                           mixer-group
                           is-sub-synth
                           sub-synths
                           sub-synth-group]
  synth-control
  (kill-synth [this] (kill (. this play-synth)))
  (kill-trg   [this] (if (. this is-inst) (do (kill play-synth)) (group-free (. this group))))
  (ctl-synth [this var value] (ctl (. this play-synth) var value))
  (free-default-buses [this] ( doseq [x (vals default-buses)] (free-bus x)))
  (free-out-bus [this] (free-bus (. this out-bus)))
  (free-secondary-out-bus [this] (free-bus (. this out-bus-secondary)))
  (apply-default-buses [this] (doseq [x default-buses] (ctl (. this play-synth) (key x) (val x))))
  (apply-default-bus [this db] (ctl (. this play-synth) db (db default-buses)))
  (apply-control-out-bus [this] (ctl (. this play-synth) :ctrl-out (. this control-out-bus)))
  (apply-out-bus [this] (ctl (. this play-synth) :out-bus 0 ))
  (apply-secondary-out-bus [this] (ctl (. this play-synth) :out-bus (. this out-bus-secondary)))
  (free-control-out-bus [this] (free-bus (. this control-out-bus)))
  (set-out-channel [this channel] (ctl (. this out-mixer ) :out-bus channel)))


(defprotocol trigger-control
  (kill-trg-group [this])
  (store-buffers [this])
  (get-or-create-pattern-buf [this new-size])
  (get-or-create-pattern-value-buf [this new-size])
  ;(get-trigger-bus [this])
  (get-trigger-value-bus [this])
  ;(get-trigger-id [this])
  ;(get-trigger-val-id [this])
  (get-pattern-vector [this])
  (get-pattern-value-vector [this]))

(defrecord triggerContainer [trigger-id
                             trigger-val-id
                             control-key
                             control-val-key
                             group
                             play-synth
                             trigger-bus
                             trigger-value-bus
                             trigger-synth
                             pattern-vector
                             pattern-value-vector
                             pattern-buf
                             pattern-value-buf
                             original-pattern-vector
                             original-pattern-value-vector]
  trigger-control
  (kill-trg-group [this] (do (group-free (. this group))
                             (free-bus trigger-bus)
                             (free-bus trigger-value-bus)
                             (doseq [x pattern-vector] (store-buffer x))
                             (doseq [x pattern-value-vector] (store-buffer x))
                             (store-buffer pattern-buf)
                             (store-buffer pattern-value-buf)))
  (store-buffers [this]      (doseq [x pattern-vector] (store-buffer x))
                             (doseq [x pattern-value-vector] (store-buffer x))
                             (store-buffer pattern-buf)
                             (store-buffer pattern-value-buf))
  (get-or-create-pattern-buf [this new-size] (let [old-size (count (. this pattern-vector))]
                                               (if (= old-size new-size)
                                                 (. this pattern-buf)
                                                 (do (store-buffer (. this pattern-buf))
                                                     (retrieve-buffer new-size)) )))
  (get-or-create-pattern-value-buf [this new-size] (let [old-size (count (. this pattern-value-vector))]
                                                     (if (= old-size new-size)
                                                       (. this pattern-value-buf)
                                                       (do (store-buffer (. this pattern-value-buf ))  (retrieve-buffer new-size)))))
  ;(get-trigger-bus [this] (. this trigger-bus))
  (get-trigger-value-bus [this] (. this trigger-value-bus))
  ;(get-trigger-id [this] (. this trigger-id))
  ;(get-trigger-val-id [this] (. this trigger-val-id))
  (get-pattern-vector [this] (. this pattern-vector))
  (get-pattern-value-vector [this] (. this pattern-value-vector)))

(defn create-synth-config [pattern-name synth-name & {:keys [issub]
                        :or {issub false}}]
  (let [out-bus           (audio-bus 1)
        out-bus-secondary (audio-bus 1)
        is_inst           (instrument? synth-name)
        synth-group       (if is_inst (:group synth-name) (group pattern-name :tail main-g))
        sub-synth-group   (group "sub-synth-group" :tail synth-group)
        mixer-group       (group "mixer-group" :tail synth-group)
        play-synth        (if is_inst (synth-name) (synth-name  [:head synth-group] :out-bus out-bus))
        out-mixer         (mono-inst-mixer [:tail mixer-group] out-bus 0 1 0.0)
        _                 (println play-synth)
        default-buses     (generate-default-buses synth-name)
        control-out-bus   (control-bus 1)
        triggers          {}
        sub-synths        {}
        synth-container   (synthContainer.
                           is_inst
                           pattern-name
                           synth-group
                           out-bus
                           out-bus-secondary
                           play-synth
                           triggers
                           synth-name
                           default-buses
                           control-out-bus
                           out-mixer
                           mixer-group
                           issub
                           sub-synths
                           sub-synth-group)]
    (apply-default-buses synth-container)
    (apply-control-out-bus synth-container)
    synth-container))


(defn create-synth-config! [pattern-name sub-pattern-name synth-name & {:keys [issub]
                        :or {issub true}}]
  (let [out-bus           (:out-bus (@synthConfig pattern-name))
        out-bus-secondary (:out-bus-secondary (@synthConfig pattern-name))
        is_inst           (instrument? synth-name)
        sub-synth-group   (:sub-synth-group (@synthConfig pattern-name))
        synth-group       (:group  (@synthConfig pattern-name))
        mixer-group       (:mixer-group  (@synthConfig pattern-name))
        play-synth        (synth-name [:tail sub-synth-group] :bus-in out-bus :out-bus out-bus)
        out-mixer         (:out-mixer (@synthConfig pattern-name))
        _                 (println play-synth)
        default-buses     (generate-default-buses synth-name)
        control-out-bus   (control-bus 1)
        triggers          {}
        sub-synths        {pattern-name (keyword sub-pattern-name)}
        synth-container   (synthContainer.
                           is_inst
                           sub-pattern-name
                           sub-synth-group
                           out-bus
                           out-bus-secondary
                           play-synth
                           triggers
                           synth-name
                           default-buses
                           control-out-bus
                           out-mixer
                           mixer-group
                           issub
                           sub-synths
                           sub-synth-group)]
    (apply-default-buses synth-container)
    (apply-control-out-bus synth-container)
    synth-container))



(defn buffer-writer [buf data] (try
                                 (buffer-write-relay! buf data)
                                 (catch Exception ex
                                   (do
                                     (store-buffer buf)
                                     (println
                                      (str "Exception while writing to buffer ")
                                      (.getException ex) )))) )

(defn create-trigger [control-key
                      control-val-key
                      synth-name
                      pattern-group
                      pattern-vector
                      pattern-value-vector]
  (let [trigger-id           (trig-id)
        trigger-val-id       (trig-id)
        trig-bus             (control-bus 1)
        trig-val-bus         (control-bus 1)
        buf-size             (count pattern-vector)
        dur-buffers          (vec (mapv (fn [x] (retrieve-buffer (count x))) pattern-vector))
        val-buffers          (vec (mapv (fn [x] (retrieve-buffer (count x))) pattern-value-vector))
        _                    (vec (mapv (fn [x y] (buffer-writer x y)) dur-buffers pattern-vector ))
        _                    (vec (mapv (fn [x y] (buffer-writer x y)) val-buffers pattern-value-vector))
        pattern-id-buf       (retrieve-buffer buf-size)
        pattern-value-id-buf (retrieve-buffer buf-size)
        dur-id-vec           (vec (map (fn [x] (buffer-id x)) dur-buffers))
        val-id-vec           (vec (map (fn [x] (buffer-id x)) val-buffers))
        _                    (buffer-writer pattern-id-buf dur-id-vec)
        _                    (buffer-writer pattern-value-id-buf val-id-vec)
        trig-group           (group (str control-key) :tail pattern-group)
        trig-synth           (trigger-generator [:tail trig-group]
                                                base-trigger-bus
                                                base-trigger-count-bus
                                                pattern-id-buf
                                                pattern-value-id-buf
                                                trig-bus
                                                trig-val-bus
                                                trigger-id
                                                trigger-val-id
                                                base-dur)]
    (try (ctl synth-name  control-key trig-bus control-val-key  trig-val-bus)
         (catch Exception ex
           (println "CTL failed during create-trigger")))
    (triggerContainer. trigger-id trigger-val-id control-key control-val-key trig-group synth-name trig-bus trig-val-bus  trig-synth  dur-buffers val-buffers pattern-id-buf pattern-value-id-buf pattern-vector pattern-value-vector)))


(defn reuse-or-create-buffer [new-buf-vec]
  (let  [new-size    (count new-buf-vec)]
    (retrieve-buffer new-size)))

;Buffer-write relays cayse timeout expections occasionally
(defn update-trigger [trigger
                      pattern-vector
                      pattern-value-vector]
  (let [trigger_old          trigger
        buf-size             (count pattern-vector)
        old-dur-buffers      (vec (:pattern-vector trigger))
        old-var-buffers      (vec (:pattern-value-vector trigger))
        ;old-dur-buffers      (vec (map (fn [x] (store-buffer x)) old-dur-buffers))
        ;old-var-buffers      (vec (map (fn [x] (store-buffer x)) old-var-buffers))
        dur-buffers          (vec (map (fn [x] (reuse-or-create-buffer x)) pattern-vector))
        val-buffers          (vec (map (fn [x] (reuse-or-create-buffer x)) pattern-value-vector))
        _                    (vec (mapv (fn [x y] (buffer-writer x y)) dur-buffers pattern-vector ))
        _                    (vec (mapv (fn [x y] (buffer-writer x y)) val-buffers pattern-value-vector))
        pattern-id-buf       (get-or-create-pattern-buf trigger buf-size)
        pattern-value-id-buf (get-or-create-pattern-value-buf trigger buf-size)
        _                    (buffer-writer pattern-id-buf       (vec (map (fn [x] (buffer-id x)) dur-buffers)))
        _                    (buffer-writer pattern-value-id-buf (vec (map (fn [x] (buffer-id x)) val-buffers)))
        trigger              (assoc trigger :pattern-vector dur-buffers)
        trigger              (assoc trigger :pattern-value-vector val-buffers)
        trigger              (assoc trigger :pattern-buf pattern-id-buf)
        trigger              (assoc trigger :pattern-value-buf pattern-value-id-buf)
        trigger              (assoc trigger :original-pattern-vector pattern-vector)
        trigger              (assoc trigger :original-pattern-value-vector pattern-value-vector)
        trig-synth           (:trigger-synth trigger)]
    (try
      (do (ctl trig-synth
               :base-pattern-buffer-in pattern-id-buf
               :base-pattern-value-buffer-in pattern-value-id-buf)
          (vec (map (fn [x] (store-buffer x)) old-dur-buffers))
          (vec (map (fn [x] (store-buffer x)) old-var-buffers))
          trigger)
         (catch Exception ex
           (do
             (println "CTL failed during update-trigger")
             (.store-buffers trigger)
             ;(vec (map (fn [x] (store-buffer x)) old-dur-buffers))
             ;(vec (map (fn [x] (store-buffer x)) old-var-buffers))
             trigger_old)))))

(defn changed? [trigger new-trigger-pattern  new-control-pattern]
  (let [old-trigger-pattern  (:original-pattern-vector trigger)
        old-control-pattern  (:original-pattern-value-vector trigger)]
    (or (not= new-trigger-pattern old-trigger-pattern) (not= new-control-pattern old-control-pattern))) true)

(defn t [synth-container control-pair]
  (let [control-key       (first control-pair)
        control-val-key   (keyword (str (name control-key) "-val"))
        control-pattern   (last control-pair)
        pattern-vectors   (generate-pattern-vector control-pattern)
        trig-pattern      (:dur pattern-vectors)
        val-pattern       (:val pattern-vectors)
        pattern-group     (:group synth-container)
        triggers          (:triggers synth-container)
        play-synth        (:play-synth synth-container)
        trigger-status    (control-key triggers)
        has-changed       (if (some? trigger-status)
                            (changed? trigger-status trig-pattern val-pattern))
        trigger           (if (some? trigger-status)
                            (if (= true has-changed)
                              (update-trigger trigger-status trig-pattern val-pattern) trigger-status)
                            (create-trigger control-key
                                            control-val-key
                                            play-synth
                                            pattern-group
                                            trig-pattern
                                            val-pattern))]
    trigger))

(defonce r "~")

(defn parse-input-vector [input]
  (let []
    (loop [xv     (seq input)
           result []]
      (if xv
        (let [fst     (first xv) ]
          (if (vector? fst) (recur (next xv) (conj result (vec (parse-input-vector fst))))
              (if (seq? fst) (recur (next xv) (apply conj result  (vec (parse-input-vector fst))))
                  (recur (next xv) (conj result fst))))) result ))))


(defn string-not-r? [x]
  (let [is-string    (string? x)
        is-r         (if is-string (= r x) false)]
    (if (and is-string (not is-r )) true false )))

(defn split-special-string [x d]
   (clojure.string/trim (last (clojure.string/split x  d 2))))

(defn match-special-case [x]
  (let [is-input-string  (string-not-r? x)
        n-string         "n"
        freq-string      "f"
        buffer-string    "b"
        ]
    (if is-input-string (cond
                          (re-matches #"n.*" x) (note (keyword (split-special-string x #"n")))
                          (re-matches #"f.*" x) (midi->hz (note (keyword (split-special-string x #"f"))))
                          (re-matches #"b.*" x) (get-sample-id (keyword (split-special-string x #"b")))) x)))


(defn special-case [input key]
  (let [y   input]
    ;(println (match-special-case y))
    (match-special-case y)
    ;; (cond
    ;;   (= key :in-buf)  (if (string-not-r? y) (get-sample-id (keyword y)) y )
    ;;   (= key :in-note) (if (string-not-r? y) (note (keyword y)) y )
    ;;   (= key :in-freq) (if (string-not-r? y) (midi->hz (note (keyword y))) y )
    ;;   :else (if (string-not-r? y) 1 y ))

    ))


(defn apply-special-cases [input special-cond]
  (let []
    (loop [xv input
           result []]
      (if xv
        (let [fst (first xv)]
          (if (vector? fst) (recur (next xv)  (conj result (vec (apply-special-cases fst special-cond))))
              (recur (next xv) (conj result (special-case fst special-cond))))) result ))))

(defn copy-key-val [input key]
  (let [value              (key input)
        value-size         (count value)
        is-string          (if (= 1 value-size) (string? (first value)) false)
        is-keyformat       (if is-string (= 2 (count (clojure.string/split (first value) #":"))) false)
        source-key         (if is-keyformat (keyword (last (clojure.string/split (first value) #":"))) nil)
        has-source         (contains? input source-key)]
    (if has-source  (source-key input)  (key input))))

(defn split-input [input]
  (let [ip      (into
                 (sorted-map)
                 (map (fn [x] {(first (first x))  (vec (parse-input-vector (last x)))})
                      (partition 2 (partition-by keyword? input))))
        specip  (into {}  (map (fn [x] {x (copy-key-val ip x)}) (keys ip)))
        ip      specip
        ip      (into {}  (map (fn [x] {x (apply-special-cases (x ip) x)}) (keys ip))) ]
    (apply conj
           (map (fn [x] {(key x)  (vec (map (fn [x] (clojure.string/replace x #"\"~\"" "~") )  (map str (val x))))})
                ip))))


(defn synth-name-check [new-sn synth-container]
  (let [sc       synth-container
        old-sn   (:synth-name synth-container)]
    (if (nil? sc) new-sn
        (if (not= old-sn new-sn) old-sn new-sn))))


;New trigger input function, allows more terse and powerfull way to create patterns. Now clojure functions such as repeat can be used directly in the input.
(defn trg ([pn sn & input]
           (let [pattern-name          (if (keyword? pn) (name pn) pn )
                 pattern-name-key      (keyword pattern-name)
                 synth-container       (pattern-name-key @synthConfig)
                 synth-name            (synth-name-check sn synth-container)
                 input                 (split-input input)
                 original-input        input
                 valid-keys            (concat [:pn :sn]  (vec (synth-args synth-name)))
                 input                 (select-keys input (vec valid-keys)) ; Make input valid, meaning remove control keys that are not present in the synth args
                 input-controls-only   input
                 initial-controls-only input-controls-only
                 input-check           (some? (not-empty input-controls-only))]
             (if
                 (= nil synth-container)
               (do (println "Synth created")
                   (swap! synthConfig assoc pattern-name-key (create-synth-config pattern-name synth-name)))
               (do (println "Synth exists")))
             (do
               (let [synth-container     (pattern-name-key @synthConfig)
                     triggers            (:triggers synth-container)
                     running-trigger-keys(keys triggers)
                     input-trigger-keys  (keys initial-controls-only)
                     triggers-not-renewd (first (diff running-trigger-keys input-trigger-keys))
                     _                   (doseq [x triggers-not-renewd]
                                           (if (some? x)
                                             (do (kill-trg-group (x triggers)) (apply-default-bus synth-container x))))
                     triggers            (apply dissoc triggers triggers-not-renewd)
                     synth-container     (assoc synth-container :triggers triggers)]
                 (swap! synthConfig assoc pattern-name-key synth-container)))
             (swap! synthConfig assoc pattern-name-key
                    (assoc (pattern-name-key @synthConfig)
                           :triggers
                           (zipmap (keys input-controls-only)
                                   (map (partial t (pattern-name-key @synthConfig)) input-controls-only))))
             pattern-name)))




(defn trg! ([pn spn sn & input]
           (let [pattern-name            (if (keyword? pn) (name pn) pn )
                 pattern-name-key        (keyword pattern-name)
                 sub-pattern-name        (if (keyword? spn) (name spn) spn )
                 sub-pattern-name-key    (keyword sub-pattern-name)
                 parent-synth-container  (pattern-name-key @synthConfig)
                 is-inst-parent-synth    (:is-inst parent-synth-container)
                 parent-sub-synths       (:sub-synths parent-synth-container)
                 synth-container         (sub-pattern-name-key @synthConfig)
                 synth-name              (synth-name-check sn synth-container)
                 is-inst-sub-synth       (instrument? sn)
                 input                   (split-input input)
                 original-input          input
                 valid-keys              (concat [:pn :spn :sn]  (vec (synth-args synth-name)))
                 input                   (select-keys input (vec valid-keys)) ; Make input valid, meaning remove control keys that are not present in the synth args
                 input-controls-only     input
                 initial-controls-only   input-controls-only
                 input-check             (some? (not-empty input-controls-only))]
             (if (and (not is-inst-parent-synth) (not is-inst-sub-synth) (not= nil parent-synth-container))
               (do (if
                       (= nil synth-container)
                     (do (println "Synth created")
                         (swap! synthConfig assoc pattern-name-key (assoc parent-synth-container :sub-synths  (assoc parent-sub-synths sub-pattern-name-key pattern-name-key)))
                         (swap! synthConfig assoc sub-pattern-name-key (create-synth-config! pattern-name-key sub-pattern-name synth-name))  )
                     (do (println "Synth exists")))
                   (do
                     (let [synth-container     (sub-pattern-name-key @synthConfig)
                           triggers            (:triggers synth-container)
                           running-trigger-keys(keys triggers)
                           input-trigger-keys  (keys initial-controls-only)
                           triggers-not-renewd (first (diff running-trigger-keys input-trigger-keys))
                           _                   (doseq [x triggers-not-renewd]
                                                 (if (some? x)
                                                   (do (kill-trg-group (x triggers)) (apply-default-bus synth-container x))))
                           triggers            (apply dissoc triggers triggers-not-renewd)
                           synth-container     (assoc synth-container :triggers triggers)]
                       (swap! synthConfig assoc sub-pattern-name-key synth-container)))
                   (swap! synthConfig assoc sub-pattern-name-key
                          (assoc (sub-pattern-name-key @synthConfig)
                                 :triggers
                                 (zipmap (keys input-controls-only)
                                         (map (partial t (sub-pattern-name-key @synthConfig)) input-controls-only))))) (println "Parent synth" pattern-name-key "does not exist or one of the inputs is an inst.") )
             sub-pattern-name)))

                                        ; Misc pattern related functions

(defn stop-pattern [pattern-name]
  (let [pattern-name-key      pattern-name ;(keyword pattern-name)
        pattern-status        (pattern-name-key @synthConfig)
        issub                 (:is-sub-synth pattern-status)
        sub-synths            (:sub-synths pattern-status)
        sub-synth-keys        (keys sub-synths)
        sub-synth-vals        (vals sub-synths)
        triggers              (vals (:triggers pattern-status))]
    (if (some? pattern-status) (do (if (not issub) (free-default-buses pattern-status))
                                   (if (not issub) (free-control-out-bus pattern-status))
                                   (if (not issub) (free-out-bus pattern-status))
                                   (if (not issub) (free-secondary-out-bus pattern-status))
                                   (doseq [x triggers] (kill-trg-group x))
                                   (if (not issub)
                                     (kill-trg pattern-status)
                                     (kill-synth pattern-status) )
                                   (if (not issub)
                                     (reset! synthConfig  (apply dissoc @synthConfig sub-synth-keys))
                                     (do
                                       (reset! synthConfig  (apply dissoc @synthConfig sub-synth-vals))
                                       (doseq [x sub-synth-keys]  (reset! synthConfig (assoc @synthConfig x  (assoc (x @synthConfig) :sub-synths  (apply dissoc (:sub-synths (x @synthConfig)) sub-synth-vals)))))))
                                   (swap! synthConfig dissoc pattern-name-key) (println "pattern" (:pattern-name pattern-status) "stopped")) (println "No such pattern") )))

(defn stp [& pattern-names]
  (doseq [x pattern-names] (stop-pattern x)))

(defn sta []
  (doseq [x  (keys @synthConfig)] (stop-pattern x)))

(defn set-out-bus [pattern-name]
  (let [pattern-name-key    pattern-name
        pattern-status (pattern-name-key @synthConfig)]
    (if (some? pattern-status) (apply-out-bus pattern-status) )))

(defn set-secondary-out-bus [pattern-name]
  (let [pattern-name-key    pattern-name
        pattern-status (pattern-name-key @synthConfig)]
    (if (some? pattern-status) (apply-secondary-out-bus pattern-status) )))


(defn set-mixer-out-channel [pattern-name channel]
  (let [pattern-name-key    pattern-name
        pattern-status (pattern-name-key @synthConfig)]
    (if (some? pattern-status) (set-out-channel pattern-status channel) )) nil)

(defn get-out-bus [pattern-name]
  (:out-bus (pattern-name @synthConfig)))

(defn get-secondary-out-bus [pattern-name]
  (:out-bus-secondary (pattern-name @synthConfig)))

(defn get-ctrl-bus [pattern-name]
  (:control-out-bus (pattern-name @synthConfig)))

(defn get-trigger-bus [pattern-name trig-name]
  (:trigger-bus (trig-name (:triggers (pattern-name @synthConfig)))))

(defn get-trigger-val-bus [pattern-name trig-name]
  (:trigger-val-bus (trig-name (:triggers (pattern-name @synthConfig)))))

(defn get-trigger-id [pattern-name trig-name]
  (:trigger-id (trig-name (:triggers (pattern-name @synthConfig)))))

(defn get-trigger-val-id [pattern-name trig-name]
  (:trigger-val-id (trig-name (:triggers (pattern-name @synthConfig)))))

(defn get-vector [pattern-name trig-name]
  (get-pattern-vector (trig-name (:triggers (pattern-name @synthConfig)))))

(defn get-value-vector [pattern-name trig-name]
  (get-pattern-value-vector (trig-name (:triggers (pattern-name @synthConfig)))))


(defn list-sub-synths [pattern-name] (let [pattern (pattern-name @synthConfig)
                                           issub   (:is-sub-synth pattern)]
                                       (if issub (println "Is a sub-synth, parent:" (keys (:sub-synths pattern)))
                                           (println "Sub synths:" (keys (:sub-synths pattern)))))
  nil)


                             ;pattern-vector
                             ;pattern-value-vector
                             ;pattern-buf
                             ;pattern-value-buf
                             ;original-pattern-vector
                             ;original-pattern-value-vector

(defn get-buffer-ids [pattern-name bt]
  (let [pattern-status (pattern-name @synthConfig)
        triggers       (:triggers pattern-status)]
    (println (count triggers))
    (map (fn [x] (bt ((first x) triggers))) triggers)
    )
  )

(defn sctl [pattern-name var value]
  (let [pattern-status (pattern-name @synthConfig)]
    (if (some? pattern-status) (ctl-synth pattern-status var value))))

(defn connect-synths [src-synth dst-synth input-name]
  (let [output-bus   (get-secondary-out-bus src-synth)]
    (sctl dst-synth input-name output-bus)
    (set-secondary-out-bus src-synth)) )

(defn lss [] (println (keys @synthConfig))  (keys @synthConfig) )

                                        ; OSC
;(var addr=NetAddr.new("127.0.0.1", 3333);  OSCdef ('/tidalplay2', { arg msg; addr.sendMsg("/play2", *msg);}, '/play2', n);)
(defn init-osc [port]
  (def oscserver (osc-server port "osc-clj"))
  (def client (osc-client "localhost" port))
  (zero-conf-on)
  (java.net.InetAddress/getLocalHost))

(defn osc-extract-tidal [msg key]
  (let [submsg  (into [] (subvec (vec (key msg)) 1))
        part     (mapv (fn [x] [(keyword (first x)) (last x)]) (partition 2 submsg))]
    (into {} part)))

                                        ;Algorithm


(defn rm-alg [pattern trigger buf-id]
  (let [trg_str        (str (name trigger) "-" (str buf-id))
        alg-key        (keyword (name pattern) trg_str)
        key-exist      (some? (alg-key @algConfig))]
    (if key-exist (do
                    (swap! algConfig dissoc alg-key)
                    (remove-event-handler alg-key) ))))

(defn alg [pattern trigger buf-id function & args]
  (let [pat-vec        (get-vector pattern trigger)
        pat-val-vec    (get-value-vector pattern trigger)
        trigger-id     (get-trigger-val-id pattern trigger)
        vec-size       (count pat-vec)
        trg_str        (str (name trigger) "-" (str buf-id))
        alg-key        (keyword (name pattern) trg_str)
        key-exist      (some? (alg-key @algConfig))]
    (println alg-key)
    (println (nth pat-vec 0))
    (if key-exist
      (do (println "Alg key exists for pattern" pattern ", trigger" trigger ", buffer" buf-id))
      (do (function trigger-id alg-key pat-vec pat-val-vec buf-id algConfig args)
          (swap! algConfig assoc alg-key alg-key )))))

                                        ;Mixer and Instrument effects functions

(defn volume! [pattern-name vol] (let [pat      (pattern-name @synthConfig)
                                       inst     (:synth-name pat)
                                       is-inst  (instrument? inst)]
                                   (if is-inst (inst-volume! inst vol) (ctl (:out-mixer pat) :volume vol) ))
  nil)


(defn pan! [pattern-name pan] (let [pat      (pattern-name @synthConfig)
                                    inst     (:synth-name pat)
                                    is-inst  (instrument? inst)]
                                   (if is-inst (inst-pan! inst pan) (ctl (:out-mixer pat) :pan pan) )) nil )


(defn fx! [pattern-name fx] (let [pat      (pattern-name @synthConfig)
                                       inst     (:synth-name pat)
                                       is-inst  (instrument? inst)]
                              (if is-inst (inst-fx! inst fx)))
  nil)


(defn clrfx! [pattern-name] (let [pat      (pattern-name @synthConfig)
                                       inst     (:synth-name pat)
                                       is-inst  (instrument? inst)]
                                 (if is-inst (clear-fx inst)))
  nil)


                                        ;Master mixer



                                        ;Start trigger
(start-trigger)
