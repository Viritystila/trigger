;Synths


(defsynth testsin [in-trg 0 in-trg-val 0 in-attack 0 in-attack-val 0 f 200 out-bus 0] (let [trg (in:kr in-trg)
                                                                                           val (in:kr in-trg-val)
                                                                                           env (env-gen (perc (in:kr in-attack-val) 0.01 1 0) :gate trg)
                                                                                           src (* env (sin-osc (* f val)))]
                                                                                       ;(out:kr dbg (in:kr in-attack-val))
                                                                                       (out out-bus src)))
