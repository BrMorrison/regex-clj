(ns regex-compiler.lexer)

(def star         {:type :star})
(def plus         {:type :plus})
(def optional     {:type :optional})
(def alternation  {:type :alternation})
(def wildcard     {:type :wildcard})
(def comma        {:type :comma})
(def dash         {:type :dash})
(def carrot       {:type :carrot})
(def dollar       {:type :dollar})
(def open-paren   {:type :open-paren})
(def close-paren  {:type :close-paren})
(def open-brace   {:type :open-brace})
(def close-brace  {:type :close-brace})
(def open-curly   {:type :open-curly})
(def close-curly  {:type :close-curly})
(defn literal [c] {:type :literal :val c})

(defn- lex-step
    "Returns the lexed token and the remaining unconsumed string."
    [s]
    (let [ch (subs s 0 1)
          remainder (subs s 1)]
        (case ch
            "*" [star remainder]
            "+" [plus remainder]
            "?" [optional remainder]
            "|" [alternation remainder]
            "." [wildcard remainder]
            "," [comma remainder]
            "-" [dash remainder]
            "^" [carrot remainder]
            "$" [dollar remainder]
            "(" [open-paren remainder]
            ")" [close-paren remainder]
            "[" [open-brace remainder]
            "]" [close-brace remainder]
            "{" [open-brace remainder]
            "}" [close-brace remainder]
            "\\" [(literal (subs remainder 0 1)) (subs remainder 1)]
            [(literal ch) remainder])))

(defn lex [regex-str]
    (loop [toks [] str regex-str]
        (if (empty? str)
            toks
            (let [[next-tok remainder] (lex-step str)]
                (recur (conj toks next-tok) remainder)))))