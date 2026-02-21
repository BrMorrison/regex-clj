(ns web.eval-renderer
    (:require [clojure.string :as str]
              [regex-compiler.instruction :as inst]))

(defn- str-state [s sp]
    ; abcd
    ;   ^
    (let [spacer    (str/join (repeat sp " "))
          indicator (str/join [spacer "^"])]
        (str/join "\n" [s indicator])))

(defn- inst-decorator [threads index instruction]
    (let [inst-str (inst/inst-assembly instruction)
          base-str (str/join " " [(str index) inst-str])]
        (if (some #{index} threads)
            (str/join " " [base-str "<="])
            base-str)))

(defn- prog-state [prog threads]
    ; 000 char a <=
    ; 001 match
    (let [inst-list (map-indexed (partial inst-decorator threads) prog)]
        (str/join "\n" (vec inst-list))))

(defn eval-repr [prog s state]
    (let [header  (str-state s (:sp state))
          divider (str/join (repeat 16 "-"))
          body    (prog-state prog (:threads state))]
        (str/join [header "\n" divider "\n\n" body])))