(ns regex-compiler.parser
    "Code for parsing an AST from a regex"
    (:require [regex-compiler.ast :as ast]
              [regex-compiler.lexer :as lex]))

(declare parse-regex)

;; Parsing Helpers
(defn- peek-type [toks]
  (when-not (empty? toks) (:type (first toks))))

(defn- next-is? [toks type] (= type (peek-type toks)))

(defn- primary-start? [toks]
    (or (next-is? toks :literal) (next-is? toks :open-paren)))

(defn extract-group
    "Given a string starting with '(' returns [inside remaining]
     Throws if the parentheses are unbalanced."
    [open-tok close-tok toks]
    (when-not (= (peek-type toks) open-tok)
        (throw (ex-info "Expected opener at start of group" {:input toks})))

    (let [n (count toks)]
        (loop [i 1 depth 1]
            (when (= i n)
                (throw (ex-info "Unterminated group" {:input toks})))

            (let [tok (:type (nth toks i))]
                (cond
                    (= tok open-tok)
                    (recur (inc i) (inc depth))

                    (= tok close-tok)
                    (if (= depth 1)
                        [(take (dec i) (rest toks)) (drop (inc i) toks)]
                        (recur (inc i) (dec depth)))

                    :else (recur (inc i) depth))))))

;; Grammar Constructions

; primary     := literal | '(' regex ')'
(defn- parse-primary [toks]
    (cond
        (next-is? toks :literal)
        [(ast/literal (:val (first toks))) (rest toks)]

        (next-is? toks :open-paren) 
        (let [[sub-regex remainder] (extract-group :open-paren :close-paren toks)]
            [(parse-regex sub-regex) remainder])

        :else (throw (ex-info "Expected literal or '('" {:actual (peek-type toks)}))))

; repetition  := primary ('*' | '+' | '?')?
(defn- parse-repetition [toks]
    (let [[parsed remainder] (parse-primary toks)]
        (cond 
            (next-is? remainder :star) [(ast/star parsed) (rest remainder)]
            (next-is? remainder :plus) [(ast/plus parsed) (rest remainder)]
            (next-is? remainder :optional) [(ast/optional parsed) (rest remainder)]
            :else [parsed remainder])))

; sequence    := repetition+
(defn- parse-sequence [toks]
    (loop [[parsed remainder] (parse-repetition toks)]
        (if (primary-start? remainder)
            ; Parse another repetition
            (let [[parsed' remainder'] (parse-repetition remainder)]
                (recur [(ast/concatenation parsed parsed') remainder']))
            [parsed remainder])))

; alternation := concatenation ('|' concatenation)*
(defn- parse-alt [toks]
    (loop [[parsed remainder] (parse-sequence toks)]
        (if (next-is? remainder :alternation)
            (let [[parsed' remainder'] (parse-sequence (rest remainder))]
                (recur [(ast/alt parsed parsed') remainder']))
            [parsed remainder])))

; regex       := alternation
(defn- parse-regex [toks] 
    (let [[parsed remainder] (parse-alt toks)]
        (if (empty? remainder)
            parsed
            (throw (ex-info "Failed to parse regex. There was a remainder after parsing." 
                            {:input toks :remainder remainder})))))

(defn parse [s]
    (parse-regex (lex/lex s)))