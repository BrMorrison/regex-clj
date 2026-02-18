(ns regex-compiler.instruction
    (:require [clojure.string :as str]))

; Assembly Instructions

(defn inst-match []
    {:op :match})

(defn inst-label [lbl]
    {:op :label :label lbl})

(defn inst-char [c]
    {:op :char :char c})

(defn inst-jmp [dest]
    {:op :jmp :dest dest})

(defn inst-split [dest1 dest2]
    {:op :split
     :dest1 dest1
     :dest2 dest2})

; Printing Functions

(defn- assembly-pass
    "Creates an assembly representation of the instruction"
    [inst]
    (case (:op inst)
        :match "match"
        :label (str (:label inst) ":")
        :char  (str "char " (:char inst))
        :jmp   (str "jmp " (:dest inst))
        :split (str "split " (:dest1 inst) " " (:dest2 inst))))

(defn assembly
    "Generates a string assembly code representation of the program"
    [prog]
    (str/join "\n" (map assembly-pass prog)))

; Reading Functions

(defn- parse-inst
    "Parses a single instruction from a line of input"
    [line]
    (let [[op & args] (str/split line #"\s+")]
        (case (str/upper-case op)
            "MATCH" (inst-match)
            "CHAR"  (inst-char (first args))
            "JMP"   (inst-jmp (parse-long (first args)))
            "SPLIT" (inst-split (parse-long (nth args 0))
                                (parse-long (nth args 1)))

            (throw (ex-info "Unrecognized instruction" {:inst op})))))

(defn parse-assembled
    "Parses an assembly program from the input string"
    [s]
    (reduce conj [] (map parse-inst (str/split-lines s))))
