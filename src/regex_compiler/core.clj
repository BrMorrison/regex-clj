(ns regex-compiler.core
    "Entry point for the regex compiler"
    (:require [regex-compiler.parser :as parser]
              [regex-compiler.codegen :as codegen]))

(defn compile-regex [regex-str] (codegen/code-gen (parser/parse regex-str)))
