(ns regex-vm.vm
    "A virtual machine for executing regular expressions")

(defrecord VMState [threads sp matched?])

(defn- inst-at [prog thread] (nth prog thread))

(defn- char-matches? [ch prog thread]
    (let [inst (inst-at prog thread)]
        (and (= :char (:op inst))
             (= (str ch) (:char inst)))))

(defn- ec-needed? [prog threads]
    "Check if the VM needs to execute an epsilon closure"
    (some #(let [op (:op (inst-at prog %))]
                (or (= :split op)
                    (= :jmp   op)))
            threads))

(defn- is-match? [prog threads]
    "Check if the VM is currently in a matched state"
    (boolean
        (some #(= :match (:op (inst-at prog %)))
              threads)))

(defn- epsilon-closure
    "Calculates the epsilon closure of the program for the initial set of threads.
     This determines which program states are reachable without consuming input."
    [prog init-threads]

    ; Work with sets to avoid duplicate threads
    (loop [worklist (set init-threads)
           visited #{}
           result #{}]
        (if (empty? worklist)
            result

            ; Pop the next thread off the queue and get the corresponding instruction
            (let [pc (first worklist)
                worklist (disj worklist pc)
                inst (nth prog pc)]

                ; Don't process it if it's a program state we've already seen
                (if (contains? visited pc) ;
                    (recur worklist visited result)
                    (let [visited (conj visited pc)]
                        (case (:op inst)
                            :jmp
                            (recur (conj worklist (:dest inst))
                                (conj visited pc)
                                result)

                            :split
                            (recur (conj worklist (:dest1 inst) (:dest2 inst))
                                (conj visited pc)
                                result)

                            (:char :match)
                            (recur worklist visited (conj result pc)))))))))

(def init-state (->VMState #{0} 0 false))

(defn step
    "Performs a single execution state on the VM and returns an updated state."
    [prog s state]
    (if (ec-needed? prog (:threads state))

        ; If the epsilon closure does something, then do that as the step. 
        (let [ec-threads (epsilon-closure prog (:threads state))]
            (->VMState ec-threads (:sp state) (is-match? prog ec-threads)))

        ; Otherwise, consume a token of input and make that the step.
        (let [sp (:sp state)
              ch (nth s sp)
              threads (keep #(when (char-matches? ch prog %) (inc %)) (:threads state))]
            (->VMState threads (inc sp) (is-match? prog threads)))))

(defn done?
    [prog s state]
    (let [threads (:threads state)]
        (or (empty? threads)
            (and (not (ec-needed? prog threads))
                 (= (count s) (:sp state))))))

(defn run
    [prog s state]
    (loop [state state]
        (if (done? prog s state)
            state
            (recur (step prog s state)))))

(defn match? [prog s]
    (:matched? (run prog s init-state)))
