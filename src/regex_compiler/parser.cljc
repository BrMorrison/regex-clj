(ns regex-compiler.parser
    "Code for parsing an AST from a regex"
    (:require [regex-compiler.ast :as ast]))

(declare parse-regex)

;; Parsing Helpers
(defn- peek [s]
  (when-not (empty? s) (subs s 0 1)))

(defn- consume [s] (subs s 1))

(defn- next-is? [s ch] (= ch (peek s)))

(defn- literal-start? [s]
  (let [c (peek s)]
    (and c (not (#{"(" ")" "+" "*" "?" "|"} c)))))

(defn- primary-start? [s]
    (or (literal-start? s) (next-is? s "(")))

(defn- extract-group
    "Given a string starting with '(' returns [inside remaining]
     Throws if the parentheses are unbalanced."
    [s]
    (when-not (and (string? s)
                   (> (count s) 0)
                   (= (subs s 0 1) "("))
        (throw (ex-info "Expected '(' at start of group" {:input s})))

    (let [n (count s)]
        (loop [i 1 depth 1]
            (when (= i n)
                (throw (ex-info "Unterminated group" {:input s})))

            (let [c (subs s i (inc i))]
                (cond
                    (= c "(")
                    (recur (inc i) (inc depth))

                    (= c ")")
                    (if (= depth 1)
                        [(subs s 1 i)
                         (subs s (inc i))]
                        (recur (inc i) (dec depth)))

                    :else (recur (inc i) depth))))))

;; Grammar Constructions

; primary     := literal | '(' regex ')'
(defn- parse-primary [s]
    (cond
        (literal-start? s)
        [(ast/literal (subs s 0 1)) (subs s 1)]

        (next-is? s "(") 
        (let [[sub-regex remainder] (extract-group s)]
            [(parse-regex sub-regex) remainder])

        :else (throw (ex-info "Expected literal or '('" {:actual (peek s)}))))

; repetition  := primary ('*' | '+' | '?')?
(defn- parse-repetition [s]
    (let [[parsed remainder] (parse-primary s)]
        (cond 
            (next-is? remainder "*") [(ast/star parsed) (consume remainder)]
            (next-is? remainder "+") [(ast/plus parsed) (consume remainder)]
            (next-is? remainder "?") [(ast/optional parsed) (consume remainder)]
            :else [parsed remainder])))

; sequence    := repetition+
(defn- parse-sequence [s]
    (loop [[parsed remainder] (parse-repetition s)]
        (if (primary-start? remainder)
            ; Parse another repetition
            (let [[parsed' remainder'] (parse-repetition remainder)]
                (recur [(ast/concat parsed parsed') remainder']))
            [parsed remainder])))

; alternation := concatenation ('|' concatenation)*
(defn- parse-alt [s]
    (loop [[parsed remainder] (parse-sequence s)]
        (if (next-is? remainder "|")
            (let [[parsed' remainder'] (parse-sequence (consume remainder))]
                (recur [(ast/alt parsed parsed') remainder']))
            [parsed remainder])))

; regex       := alternation
(defn parse-regex [s] 
    (let [[parsed remainder] (parse-alt s)]
        (if (empty? remainder)
            parsed
            (throw (ex-info "Failed to parse regex. There was a remainder after parsing." 
                            {:input s :remainder remainder})))))
