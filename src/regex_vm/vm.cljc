(ns regex-vm.vm
    "A virtual machine for executing regular expressions")

(defrecord VMThread [pc])

(defn- advance [thread] (->VMThread (inc (:pc thread))))

(defn- inst-at [prog thread] (nth prog (:pc thread)))

(defn- char-matches? [ch prog thread]
    (let [inst (inst-at prog thread)]
        (and (= :char (:op inst))
             (= (str ch) (:char inst)))))

(defn- is-match? [prog thread] (= :match (:op (inst-at prog thread))))

(defn- epsilon-closure
    "Calculates the epsilon closure of the program for the initial set of threads.
     This determines which program states are reachable without consuming input."
    [prog init-threads]
    (loop [worklist (set (map #(:pc %) init-threads))
           visited #{}
           result #{}]
        (if (empty? worklist)
            (set (map #(->VMThread %) result))
            (let [pc (first worklist)
                worklist (disj worklist pc)
                inst (nth prog pc)]
                (if (contains? visited pc)
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

(defn- step-char [prog initial-threads ch]
    "Executes the regular expression matching for a single character of input
     based on the initial program threads."
    (let [threads (epsilon-closure prog initial-threads)
          match-found (some #(= :match (:op (inst-at prog %))) threads)]
        (if match-found
            threads
            (epsilon-closure prog
                             (keep #(when (char-matches? ch prog %) (advance %)) threads)))))

(defn match? [s prog]
    "Checks if the provided string is matched by the compiled regex program."
    (let [ prog (vec prog)
           initial-threads (epsilon-closure prog #{(->VMThread 0)})]
        (loop [threads initial-threads
               chars (seq s)]
            (cond
                (some (partial is-match? prog) threads) true
                (empty? threads) false
                (empty? s) false
                :else (recur (step-char prog threads (first chars))
                             (rest chars))))))
