(ns regex-compiler.codegen
    "Code generation that converts an AST into code for the regex VM"
    (:require [regex-compiler.ast :as ast]
              [regex-compiler.instruction :as inst]))

(defn- generate
    "Generates code for the given AST node at the given program counter value."
    [node]
    (case (:type node)

        ; a -> char a
        :literal
        [(inst/inst-char (:char node))]
         

        ; e1e2 -> code for e1
        ;         code for e2
        :concat
        (concat (generate (:left node))
                (generate (:right node)))

        ; e1|e2 ->     split L1 L2
        ;          L1: code for e1
        ;              jmp L3
        ;          L2: code for e2
        ;          L3:
        :alt
        (let [l1 (gensym "L")
              l2 (gensym "L")
              l3 (gensym "L")]
            (concat [(inst/inst-split l1 l2)
                     (inst/inst-label l1)]
                    (generate (:left node))
                    [(inst/inst-jmp l3)
                     (inst/inst-label l2)]
                    (generate (:right node))
                    [(inst/inst-label l3)]))

        ; e? ->     split L1 L2
        ;       L1: code for e
        ;       L2:
        :optional
        (let [l1 (gensym "L")
              l2 (gensym "L")]
            (concat [(inst/inst-split l1 l2)
                     (inst/inst-label l1)]
                    (generate (:expr node))
                    [(inst/inst-label l2)]))

        ; e* -> L1: split L2 L3
        ;       L2: code for e
        ;           jmp L1
        ;       L3:
        :star
        (let [l1 (gensym "L")
              l2 (gensym "L")
              l3 (gensym "L")]
            (concat [(inst/inst-label l1)
                     (inst/inst-split l2 l3)
                     (inst/inst-label l2)]
                    (generate (:expr node))
                    [(inst/inst-jmp l1)
                     (inst/inst-label l3)]))

        ; e+ -> L1: code for e
        ;           split L1 L2
        ;       L2:
        :plus
        (let [l1 (gensym "L")
              l2 (gensym "L")]
            (concat [(inst/inst-label l1)]
                    (generate (:expr node))
                    [(inst/inst-split l1 l2)
                     (inst/inst-label l2)]))))

(defn code-gen [tree] 
    (conj (vec (generate tree)) (inst/inst-match)))
