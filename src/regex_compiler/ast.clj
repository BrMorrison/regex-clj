(ns regex-compiler.ast
    "Abstract Syntax Tree node definitions for the regex compiler.")

; Helper functions
(defn literal [c]
    {:ast/type :regex.ast/literal
     :char c})

(defn concat [l r]
    {:ast/type :regex.ast/concat
     :left l
     :right r})

(defn alt [l r]
    {:ast/type :regex.ast/alt
     :left l
     :right r})

(defn optional [e]
    {:ast/type :regex.ast/optional
     :expr e})

(defn star [e]
    {:ast/type :regex.ast/star
     :expr e})

(defn plus [e]
    {:ast/type :regex.ast/plus
     :expr e})
