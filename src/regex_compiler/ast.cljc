(ns regex-compiler.ast
    "Abstract Syntax Tree node definitions for the regex compiler.")

; Helper functions
(defn literal [c]
    {:type :literal
     :char c})

(defn concatenation [l r]
    {:type :concat
     :left l
     :right r})

(defn alt [l r]
    {:type :alt
     :left l
     :right r})

(defn optional [e]
    {:type :optional
     :expr e})

(defn star [e]
    {:type :star
     :expr e})

(defn plus [e]
    {:type :plus
     :expr e})
