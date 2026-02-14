(ns regex-compiler.core
    "Entry point for the regex compiler"
    (:require [regex-compiler.parser :as parser]
              [regex-compiler.codegen :as codegen]
              [regex-compiler.assembler :as asm]))

(defn compile-regex [regex-str] (asm/assemble (codegen/code-gen (parser/parse regex-str))))
