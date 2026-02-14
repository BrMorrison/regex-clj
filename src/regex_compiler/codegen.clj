(ns regex-compiler.codegen
    "Code generation that converts an AST into code for the regex VM"
    (:require [regex-compiler.ast :as ast]))

; Functions for generating individual instructions
(defn- gen-match [pc] (format "%03d match\n" pc))
(defn- gen-char  [pc c] (format "%03d char %c\n" pc c))
(defn- gen-jmp   [pc dest] (format "%03d jmp %d\n" pc dest))
(defn- gen-split [pc dest1 dest2] (format "%03d split %d %d\n" pc dest1 dest2))

(defn- generate
    "Generates code for the given AST node at the given program counter value.
    Returns the generated code and the updated program counter value"
    [node pc]
    (case (:ast/type node)

        ; a -> char a
        :regex.ast/literal
        [(gen-char pc (:char node))
         (inc pc)]

        ; e1e2 -> code for e1
        ;         code for e2
        :regex.ast/concat
        (let [[code1 pc1] (generate (:left node) pc)
              [code2 pc2] (generate (:right node) pc1)]
            [(str code1 code2) pc2])

        ; e1|e2 ->     split L1 L2
        ;          L1: code for e1
        ;              jmp L3
        ;          L2: code for e2
        ;          L3:
        :regex.ast/alt
        (let [pc1 (inc pc)
              [code2 pc2] (generate (:left node) pc1)
              pc3 (inc pc2)
              [code4 pc4] (generate (:right node) pc3)]
            [(str (gen-split pc pc1 pc3)
                  code2
                  (gen-jmp pc2 pc4)
                  code4)
             pc4])

        ; e? ->     split L1 L2
        ;       L1: code for e
        ;       L2:
        :regex.ast/optional
        (let [pc1 (inc pc)
              [code2 pc2] (generate (:expr node) pc1)]
            [(str (gen-split pc pc1 pc2) code2)
              pc2])

        ; e* -> L1: split L2 L3
        ;       L2: code for e
        ;           jmp L1
        ;       L3:
        :regex.ast/star
        (let [pc1 (inc pc)
              [code2 pc2] (generate (:expr node) pc1)
              pc3 (inc pc2)]
            [(str (gen-split pc pc1 pc3)
                  code2
                  (gen-jmp pc2 pc))
             pc3])

        ; e+ -> L1: code for e
        ;           split L1 L3
        ;       L3:
        :regex.ast/plus
        (let [[code1 pc1] (generate (:expr node) pc)
              pc2 (inc pc1)]
            [(str code1 (gen-split pc1 pc pc2))
             pc2])))

(defn code-gen [tree] 
    (let [[code pc] (generate tree 0)]
        (str code (gen-match pc))))
