(ns regex-vm.vm-bt
    "A backtracking virtual machine for executing regular expressions")

(defrecord VMThread [pc sp])
(defrecord VMState [active-thread threads])

; ========== State Helpers ===========
(defn- inst-at [prog thread] (nth prog (:pc thread)))
(defn- cur-char [s vm-state] (nth s (get-in vm-state [:active-thread :sp])))
(defn- cur-inst [prog vm-state] (inst-at prog (:active-thread vm-state)))
(defn- cur-op [prog vm-state] (:op (cur-inst prog vm-state)))

(defn- next-thread [vm-state]
    (when-let [thread (peek (:threads vm-state))]
        (->VMState thread (pop (:threads vm-state)))))

(defn- add-thread [new-thread vm-state]
    (->VMState (:active-thread vm-state)
               (conj (:threads vm-state) new-thread)))

(defn- set-pc [new-pc vm-state]
    (assoc-in vm-state [:active-thread :pc] new-pc))

(defn- inc-pc [vm-state]
    (update-in vm-state [:active-thread :pc] inc))

(defn- inc-sp [vm-state]
    (update-in vm-state [:active-thread :sp] inc))

; ========== Functions for Ops ===========
(defn- handle-match [vm-state] (next-thread vm-state))
(defn- handle-char  [vm-state inst ch]
    (if (= (str ch) (:char inst))
        (-> vm-state inc-pc inc-sp)
        (next-thread vm-state)))
(defn- handle-jump  [vm-state inst] (set-pc (:dest inst) vm-state))
(defn- handle-split [vm-state inst]
    (->> vm-state
         (set-pc (:dest1 inst))
         (add-thread (->VMThread (:dest2 inst) (get-in vm-state [:active-thread :sp])))))

; ========== Public Definitions ===========
(def init-state (->VMState (->VMThread 0 0) []))

(defn step
    [prog s vm-state]
    (let [inst (cur-inst prog vm-state)
          ch   (cur-char s vm-state)]
        (case (:op inst)
            :match (handle-match vm-state)
            :char  (handle-char  vm-state inst ch)
            :jmp   (handle-jump  vm-state inst)
            :split (handle-split vm-state inst))))

(defn matched? [prog s vm-state] 
    ; A program is a match if the current instruction is "match" and there's no more input
    (and (= :match (cur-op prog vm-state))
         (= (count s) (get-in vm-state [:active-thread :sp]))))

(defn done?
    [prog s vm-state]
    (or (nil? vm-state)
        (matched? prog s vm-state)))

(defn run
    [prog s state]
    (loop [state state]
        (if (done? prog s state)
            state
            (recur (step prog s state)))))
