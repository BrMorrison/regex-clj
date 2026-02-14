(ns regex-compiler.parser
    "Code for parsing an AST from a regex"
    (:require [regex-compiler.ast :as ast]))

(defn- append-literal [ast lit]
    (case (:ast/type ast)
        nil lit
        :regex.ast/literal (ast/concat ast lit)
        :regex.ast/concat (ast/concat (:left ast)
                                      (append-literal (:right ast) lit))
        :regex.ast/alt (ast/alt (:left ast)
                                (append-literal (:right ast) lit))
        (:regex.ast/optional
         :regex.ast/star
         :regex.ast/plus) (ast/concat ast lit)))

(defn- apply-modifier [ast mod-fn]
    (case (:ast/type ast)
        :regex.ast/literal (mod-fn ast)
        :regex.ast/concat (ast/concat (:left ast)
                                      (apply-modifier (:right ast) mod-fn))
        :regex.ast/alt (ast/alt (:left ast)
                                (apply-modifier (:right ast) mod-fn))))

(defn- parse-next
    "Parse the next character in the regex and update the AST"
    [ast ch]
    (case ch
        \? (apply-modifier ast ast/optional)
        \* (apply-modifier ast ast/star)
        \+ (apply-modifier ast ast/plus)
        \| (ast/alt ast nil)
        (append-literal ast (ast/literal ch))))

(defn parse
    "Parse a regex string into an AST"
    [regex-str]
    (reduce parse-next nil regex-str))
