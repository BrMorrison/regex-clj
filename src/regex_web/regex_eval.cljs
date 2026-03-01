(ns regex-web.regex-eval
    (:require [clojure.math :as math]
              [clojure.string :as string]
              [regex-compiler.instruction :as inst]
              [regex-vm.vm-bt :as vm]))

(defrecord EvalState [prog input vm-state])

;; ---------------------------------------------------------------------
;; Access Helpers
;; ---------------------------------------------------------------------

(defn done? [state]
    (vm/done? (:prog state) (:input state) (:vm-state state)))

(defn matched? [state]
    (vm/matched? (:prog state) (:input state) (:vm-state state)))

;; ---------------------------------------------------------------------
;; Execution Helpers
;; ---------------------------------------------------------------------

(defn init [prog input]
    (->EvalState prog input vm/init-state))

(defn step [state]
    (let [prog       (:prog state)
          input      (:input state)
          eval-state (:vm-state state)
          next-state (vm/step prog input eval-state)]
        (->EvalState prog input next-state)))

(defn run [state]
    (let [prog       (:prog state)
          input      (:input state)
          eval-state (:vm-state state)
          final-state (vm/run prog input eval-state)]
        (->EvalState prog input final-state)))

;; ---------------------------------------------------------------------
;; State renderer
;; ---------------------------------------------------------------------

(defn- padded-index
    "Return an integer left-padded with zeros based on the number of
     instructions in the program."
    [width index]
    (let [padding (string/join (repeat width "0"))
          padded  (string/join [padding index])
          start   (- (count padded) width)]
        (subs padded start)))

(defn- input-html
    "Renders the input string with the current scan position highlighted."
    [s sp]
    (let [chars (map-indexed
                    (fn [i ch]
                        (if (= i sp)
                            (str "<span class=\"sp-active\">" ch "</span>")
                            (str "<span>" ch "</span>")))
                    s)
          ;; Cursor shown after the last char if sp = (count s)
          cursor (when (= sp (count s))
                  "<span class=\"sp-active sp-end\">∎</span>")]
        (str "<div class=\"vm-input\">"
             (string/join chars)
             cursor
             "</div>")))

(defn- inst-html
    "Renders a single instruction row, highlighted if it's the next instruction."
    [pad-width active-pc index instruction]
    (let [inst-str (inst/inst-assembly instruction)
          idx-str  (padded-index pad-width index)
          active?  (= index active-pc)
          cls      (if active? "inst-row inst-active" "inst-row")]
        (str "<div class=\"" cls "\">"
             "<span class=\"inst-index\">" idx-str "</span>"
             "<span class=\"inst-body\">" inst-str "</span>"
             (when active? "<span class=\"inst-arrow\">◀</span>")
             "</div>")))

(defn- prog-html
    [prog pc]
    (let [pad-width (max (math/ceil (math/log10 (count prog))) 3)
          rows      (map-indexed (partial inst-html pad-width pc) prog)]
        (str "<div class=\"vm-prog\">" (string/join rows) "</div>")))

(defn- thread-row-html [index thread active?]
  (let [cls (if active? "thread-row thread-active" "thread-row")]
    (str "<div class=\"" cls "\">"
         "<span class=\"thread-index\">" (if active? "▶" (str "#" index)) "</span>"
         "<span class=\"thread-pc\">pc=" (:pc thread) "</span>"
         "<span class=\"thread-sp\">sp=" (:sp thread) "</span>"
         "</div>")))

(defn render-thread-stack-html [state]
  (let [vm-st (:vm-state state)]
    (if (nil? vm-st)
      "<div class=\"thread-list\"></div>"
      (let [active  (:active-thread vm-st)
            waiting (reverse (:threads vm-st))]
        (str "<div class=\"thread-list\">"
             (thread-row-html nil active true)
             (string/join (map-indexed #(thread-row-html %1 %2 false) waiting))
             "</div>")))))

(defn render-html [state]
    (let [prog    (:prog state)
          s       (:input state)
          vm-st   (:vm-state state)
          thread  (:active-thread vm-st)
          sp      (:sp thread)
          pc      (:pc thread)]
        (str (input-html s sp)
             "<hr class=\"vm-divider\">"
             (prog-html prog pc))))
