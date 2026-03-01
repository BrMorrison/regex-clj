(ns regex-web.regex-eval
    (:require [clojure.math :as math]
              [clojure.string :as string]
              [regex-compiler.instruction :as inst]
              [regex-vm.vm :as vm]))

(defrecord EvalState [prog input vm-state])

;; ---------------------------------------------------------------------
;; Access Helpers
;; ---------------------------------------------------------------------

(defn done? [state]
    (vm/done? (:prog state) (:input state) (:vm-state state)))

(defn matched? [state]
    (:matched? (:vm-state state)))

;; ---------------------------------------------------------------------
;; Execution Helpers
;; ---------------------------------------------------------------------

(defn init [prog input]
    (->EvalState prog input vm/init-state))

(defn step [state]
    (let [prog       (:prog state)
          input      (:input state)
          eval-state (:vm-state state)
          next-state (vm/step prog input eval-state)]
        (->EvalState prog input next-state)))

(defn run [state]
    (let [prog       (:prog state)
          input      (:input state)
          eval-state (:vm-state state)
          final-state (vm/run prog input eval-state)]
        (->EvalState prog input final-state)))

;; ---------------------------------------------------------------------
;; State renderer
;; ---------------------------------------------------------------------

(defn- str-state 
    "Creates a representation of where the evaluator is in processing the input
     string. Does not work with multi-line strings."
    [s sp]
    (let [spacer    (string/join (repeat sp " "))
          indicator (string/join [spacer "^"])]
        (string/join "\n" [s indicator])))

(defn- padded-index
    "Return an integer left-padded with zeros based on the number of
     instructions in the program."
    [width index]
    (let [padding (string/join (repeat width "0"))
          padded  (string/join [padding index])
          start   (- (count padded) width)]
        (subs padded start)))

(defn- inst-decorator
    "Decorates an instruction with an index and an index if it's an active
     instruction. e.g. `001 char a <=`"
    [pad-width threads index instruction]
    (let [inst-str (inst/inst-assembly instruction)
          base-str (string/join " " [(padded-index pad-width index) inst-str])]
        (if (some #{index} threads)
            (string/join " " [base-str "<="])
            base-str)))

(defn- prog-state
    "Creates a string representation of the program state."
    [prog threads]
    (let [pad-width (math/ceil (math/log10 (count prog)))
          pad-width (max pad-width 3)
          inst-list (map-indexed (partial inst-decorator pad-width threads) prog)]
        (string/join "\n" (vec inst-list))))

(defn repr [state]
    (let [prog    (:prog state)
          s       (:input state)
          state   (:vm-state state)
          header  (str-state s (:sp state))
          divider (string/join (repeat 16 "-"))
          body    (prog-state prog (:threads state))]
        (string/join [header "\n" divider "\n\n" body])))

