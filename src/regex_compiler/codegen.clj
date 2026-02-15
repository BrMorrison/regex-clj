(ns regex-compiler.codegen
    "Code generation that converts an AST into code for the regex VM"
    (:require [regex-compiler.ast :as ast]
              [clojure.string :as str]))

; Functions for generating individual instructions
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

(defn- generate
    "Generates code for the given AST node at the given program counter value."
    [node]
    (case (:ast/type node)

        ; a -> char a
        :regex.ast/literal
        [(inst-char (:char node))]
         

        ; e1e2 -> code for e1
        ;         code for e2
        :regex.ast/concat
        (concat (generate (:left node))
                (generate (:right node)))

        ; e1|e2 ->     split L1 L2
        ;          L1: code for e1
        ;              jmp L3
        ;          L2: code for e2
        ;          L3:
        :regex.ast/alt
        (let [l1 (gensym "L")
              l2 (gensym "L")
              l3 (gensym "L")]
            (concat [(inst-split l1 l2)
                     (inst-label l1)]
                    (generate (:left node))
                    [(inst-jmp l3)
                     (inst-label l2)]
                    (generate (:right node))
                    [(inst-label l3)]))

        ; e? ->     split L1 L2
        ;       L1: code for e
        ;       L2:
        :regex.ast/optional
        (let [l1 (gensym "L")
              l2 (gensym "L")]
            (concat [(inst-split l1 l2)
                     (inst-label l1)]
                    (generate (:expr node))
                    [(inst-label l2)]))

        ; e* -> L1: split L2 L3
        ;       L2: code for e
        ;           jmp L1
        ;       L3:
        :regex.ast/star
        (let [l1 (gensym "L")
              l2 (gensym "L")
              l3 (gensym "L")]
            (concat [(inst-label l1)
                     (inst-split l2 l3)
                     (inst-label l2)]
                    (generate (:expr node))
                    [(inst-jmp l1)
                     (inst-label l3)]))

        ; e+ -> L1: code for e
        ;           split L1 L2
        ;       L2:
        :regex.ast/plus
        (let [l1 (gensym "L")
              l2 (gensym "L")]
            (concat [(inst-label l1)]
                    (generate (:expr node))
                    [(inst-split l1 l2)
                     (inst-label l2)]))))

(defn code-gen [tree] 
    (conj (vec (generate tree)) (inst-match)))

(defn- assembly-pass
    "Creates an assembly representation of the instruction"
    [inst]
    (case (:op inst)
        :match "match"
        :label (format "%s:" (:label inst))
        :char  (format "char %c" (:char inst))
        :jmp   (format "jmp %s" (:dest inst))
        :split (format "split %s %s"
                       (:dest1 inst)
                       (:dest2 inst))))

(defn assembly
    "Generates a string assembly code representation of the program"
    [prog]
    (str/join "\n" (map assembly-pass prog)))
