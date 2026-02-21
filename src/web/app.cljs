(ns web.app 
    "Web app for using the regex compiler and interpreter"
    (:require [regex-compiler.parser :as parser]
              [regex-compiler.codegen :as codegen]
              [regex-compiler.assembler :as assembler]
              [regex-compiler.instruction :as inst]
              [regex-vm.vm :as vm]
              [web.eval-renderer :as render]))

;; ---------------------------------------------------------------------
;; DOM Helpers
;; ---------------------------------------------------------------------

(defn by-id [id]
    (.getElementById js/document id))

(defn set-value! [obj v]
    (set! (.-value obj) v))

(defn set-text! [obj v]
    (set! (.-textContent obj) v))

(defn enable! [obj]
    (set! (.-disabled obj) false))

(defn disable! [obj]
    (set! (.-disabled obj) true))

;; ---------------------------------------------------------------------
;; Page Elements
;; ---------------------------------------------------------------------

; -- Text Inputs --
(def regex-input    (by-id "regex-input"))
(def string-input   (by-id "string-input"))
(def assembly-input (by-id "assembly-input"))

; -- Text Outputs --
(def parsed-output    (by-id "parsed-output"))
(def compiled-output  (by-id "compiled-output"))
(def assembled-output (by-id "assembled-output"))
(def evaluator-output (by-id "evaluator-output"))

; -- Success/Error Messages --
(def compile-error (by-id "compile-error"))
(def match-error   (by-id "match-error"))
(def match-success (by-id "match-success"))

; -- Buttons --
(def compile-btn (by-id "compile-btn"))
(def match-btn   (by-id "match-btn"))
(def start-btn   (by-id "start-btn"))
(def reset-btn   (by-id "reset-btn"))
(def step-btn    (by-id "step-btn"))
(def run-btn     (by-id "run-btn"))

;; ---------------------------------------------------------------------
;; Page Helpers
;; ---------------------------------------------------------------------

(defn clear-compile-output! []
    (set-text! compile-error "")
    (set-value! parsed-output "")
    (set-value! compiled-output "")
    (set-value! assembled-output ""))

(defn run-safe [f out]
    (try 
        (f)
        (catch js/Error e
            (js/console.error e)
            (set-text! out (.-message e)))))


;; ---------------------------------------------------------------------
;; Compilation
;; ---------------------------------------------------------------------

(defn compile-regex! []

    (clear-compile-output!)

    (let [regex  (.-value regex-input)
            ast  (parser/parse-regex regex)
            prog (codegen/code-gen ast)
            asm  (assembler/assemble prog)]
        (set-value! parsed-output (js/JSON.stringify (clj->js ast) nil 2))
        (set-value! compiled-output (inst/assembly prog))
        (set-value! assembled-output (inst/assembly asm))
        (set-value! assembly-input (inst/assembly asm))))

;; ---------------------------------------------------------------------
;; Evaluation
;; ---------------------------------------------------------------------

(defn clear-eval-output! []
    (set-text! match-error "")
    (set-text! match-success "")
    (set-value! evaluator-output ""))

(defn read-evaluator-inputs []
    [(inst/parse-assembled (.-value assembly-input))
     (.-value string-input)])

(defn set-eval-state! [state]
    (def eval-state state))

(defn set-eval-output! [state]
    ; Update the div based on the result of the match
    (if (:matched? state)
        (set-text! match-success "Matched!")
        (set-text! match-error "No Match!")))

(defn update-state! [prog s state]
    (set-value! evaluator-output (render/eval-repr prog s state))
    (set-eval-state! state))

(defn reset-eval! []
    (let [[prog s] (read-evaluator-inputs)]
        (clear-eval-output!)
        (update-state! prog s vm/init-state)))

(defn start-eval! []
    (disable! start-btn)
    (disable! compile-btn)
    (enable!  reset-btn)
    (enable!  step-btn)
    (enable!  run-btn)
    (set! (.-readonly string-input) true)
    (set! (.-readonly assembly-input) true)
    (reset-eval!))

(defn stop-eval! []
    (disable! reset-btn)
    (disable! step-btn)
    (disable! run-btn)
    (enable!  start-btn)
    (enable!  compile-btn)
    (set! (.-readonly string-input) false)
    (set! (.-readonly assembly-input) false))

(defn check-match! []
    (clear-eval-output!)

    (let [[program s] (read-evaluator-inputs)
            final-state (vm/run program s vm/init-state)]
        (set-eval-output! final-state)))

(defn run-evaluator! []

    (clear-eval-output!)

    (let [[program s] (read-evaluator-inputs)
            final-state (vm/run program s eval-state)]

        (set-eval-output! final-state)
        (stop-eval!)))

(defn step-evaluator! []

    (clear-eval-output!)

    (let [[program s] (read-evaluator-inputs)
          next-state (vm/step program s eval-state)]

        (update-state! program s next-state)

        (when (vm/done? program s next-state)
            (do (set-eval-output! next-state)
                (stop-eval!)))))

;; ---------------------------------------------------------------------
;; Entry Point
;; ---------------------------------------------------------------------

(.addEventListener
    compile-btn
    "click"
    #(run-safe compile-regex! compile-error))

(.addEventListener
    match-btn
    "click"
    #(run-safe check-match! match-error))

(.addEventListener
    start-btn
    "click"
    start-eval!)

(.addEventListener
    reset-btn
    "click"
    reset-eval!)

(.addEventListener
    step-btn
    "click"
    #(run-safe step-evaluator! match-error))

(.addEventListener
    run-btn
    "click"
    #(run-safe run-evaluator! match-error))
