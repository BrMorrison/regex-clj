(ns web.app 
    "Web app for using the regex compiler and interpreter"
    (:require [regex-compiler.parser :as parser]
              [regex-compiler.codegen :as codegen]
              [regex-compiler.assembler :as assembler]
              [regex-compiler.instruction :as inst]
              [regex-vm.vm :as vm]))

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

(defn clear-match-output! []
    (set-text! "match-error" "")
    (set-text! "match-success" ""))

(defn clear-compile-output! []
    (clear-match-output!)
    (set-text! "error" "")
    (set-value! "parsed-output" "")
    (set-value! "compiled-output" "")
    (set-value! "assembled-output" ""))

(defn compile-regex! []

    (clear-compile-output!)

    (try
        (let [regex    (value-of "regex-input")
              ast      (parser/parse-regex regex)
              prog     (codegen/code-gen ast)
              asm      (assembler/assemble prog)]
            (set-value! "parsed-output" (js/JSON.stringify (clj->js ast) nil 2))
            (set-value! "compiled-output" (inst/assembly prog))
            (set-value! "assembled-output" (inst/assembly asm)))

        (catch js/Error e
            (set-text! "error" (.-message e)))))

(defn evaluate-regex! []

    (clear-match-output!)

    (try
        (let [s        (value-of "string-input")
              program  (inst/parse-assembled (value-of "assembled-output"))]

            ; Update the div based on the result of the match
            (if (vm/match? s program)
                (set-text! "match-success" "Matched!")
                (set-text! "match-error" "No Match!")))

    (catch :default e
        (set-text!
            "match-error"
            (str "Error: " (.-message e))))))


;; ---------------------------------------------------------------------
;; Entry Point
;; ---------------------------------------------------------------------

(.addEventListener
    (by-id "compile-btn")
    "click"
    compile-regex!)

(.addEventListener
    (by-id "match-btn")
    "click"
    evaluate-regex!)
