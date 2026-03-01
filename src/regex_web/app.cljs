(ns regex-web.app 
    "Web app for using the regex compiler and interpreter"
    (:require [regex-compiler.parser :as parser]
              [regex-compiler.codegen :as codegen]
              [regex-compiler.assembler :as assembler]
              [regex-compiler.instruction :as inst]
              [regex-vm.vm :as vm]
              [regex-web.regex-eval :as eval]))

(defonce eval-state (atom nil))

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
(def start-btn   (by-id "start-btn"))
(def step-btn    (by-id "step-btn"))
(def run-btn     (by-id "run-btn"))

;; ---------------------------------------------------------------------
;; Page Helpers
;; ---------------------------------------------------------------------

(defn clear-compile-output! []
    (set-text! compile-error "")
    (set-text! parsed-output "")
    (set-text! compiled-output "")
    (set-text! assembled-output ""))

(defn run-safe [f out]
    (try 
        (f)
        (catch js/Error e
            (js/console.error e)
            (set-text! out (.-message e)))))

(defn safe-handler [f error-node]
    #(run-safe f error-node))

;; ---------------------------------------------------------------------
;; Compilation
;; ---------------------------------------------------------------------

(defn compile-regex! []

    (clear-compile-output!)

    (let [regex  (.-value regex-input)
            ast  (parser/parse-regex regex)
            prog (codegen/code-gen ast)
            asm  (assembler/assemble prog)]
        (set-text! parsed-output (js/JSON.stringify (clj->js ast) nil 2))
        (set-text! compiled-output (inst/assembly prog))
        (set-text! assembled-output (assembler/encode-str asm))
        (set-value! assembly-input (assembler/encode-str asm))))

;; ---------------------------------------------------------------------
;; Evaluation
;; ---------------------------------------------------------------------

(defn clear-eval-output! []
    (set-text! match-error "")
    (set-text! match-success "")
    (set-text! evaluator-output ""))

(defn render-eval-state! [state]
    (clear-eval-output!)
    (set-text! evaluator-output (eval/repr state))
    (when (eval/done? state)
        (if (eval/matched? state)
            (set-text! match-success "✓ Matched!")
            (set-text! match-error   "× No Match!"))))



(defn stop-eval! []
    (disable! step-btn)
    (disable! run-btn)
    (enable!  start-btn)
    (enable!  compile-btn)
    (set! (.-readonly string-input) false)
    (set! (.-readonly assembly-input) false))


(defn update-state! [state]
    (reset! eval-state state)
    (render-eval-state! state)
    (when (eval/done? state)
        (stop-eval!)))


(defn reset-eval! []
    (let [prog (inst/parse-machine-code (.-value assembly-input))
          s    (.-value string-input)]
        (clear-eval-output!)
        (update-state! (eval/init prog s))))

(defn start-eval! []
    (disable! start-btn)
    (disable! compile-btn)
    (enable!  step-btn)
    (enable!  run-btn)
    (set! (.-readonly string-input) true)
    (set! (.-readonly assembly-input) true)
    (run-safe reset-eval! match-error))


(defn run-evaluator! []
    (update-state! (eval/run @eval-state)))

(defn step-evaluator! []
    (update-state! (eval/step @eval-state)))

;; ---------------------------------------------------------------------
;; Entry Point
;; ---------------------------------------------------------------------

(.addEventListener
    compile-btn
    "click"
    (safe-handler compile-regex! compile-error))

(.addEventListener
    start-btn
    "click"
    (safe-handler start-eval! match-error))

(.addEventListener
    step-btn
    "click"
    (safe-handler step-evaluator! match-error))

(.addEventListener
    run-btn
    "click"
    (safe-handler run-evaluator! match-error))

; Stop evaluation to make sure the page loads in a known state
(stop-eval!)
