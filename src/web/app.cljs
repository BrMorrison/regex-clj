(ns web.app 
    "Web app for using the regex compiler and interpreter"
    (:require [regex-compiler.parser :as parser]
              [regex-compiler.codegen :as codegen]
              [regex-compiler.assembler :as assembler]))

;; ---------------------------------------------------------------------
;; DOM Helpers
;; ---------------------------------------------------------------------

(defn by-id [id]
    (.getElementById js/document id))

(defn value-of [id]
    (.-value (by-id id)))

(defn set-value! [id v]
    (set! (.-value (by-id id)) v))

(defn set-text! [id v]
    (set! (.-textContent (by-id id)) v))

;; ---------------------------------------------------------------------
;; Compilation Pipeline
;; ---------------------------------------------------------------------

(defn compile-regex! []

    ; Clear output
    (set-text! "error" "")
    (set-value! "output" "")

    (try
        (let [regex    (value-of "regex-input")
              ast      (parser/parse-regex regex)
              prog     (codegen/code-gen ast)
              asm      (assembler/assemble prog)
              asm-text (codegen/assembly asm)]
            (set-value! "output" asm-text))

        (catch js/Error e
            ;; ex-info shows up as js/Error in CLJS
            (set-text! "error" (.-message e)))))

;; ---------------------------------------------------------------------
;; Entry Point
;; ---------------------------------------------------------------------

(.addEventListener
    (by-id "compile-btn")
    "click"
    compile-regex!)