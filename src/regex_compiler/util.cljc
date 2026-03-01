(ns regex-compiler.util)

(defn char->int
  "Convert a single-character string to its integer code point."
  [s]
  {:pre [(string? s) (= 1 (count s))]}
  #?(:clj  (int (.charAt ^String s 0))
     :cljs (.charCodeAt s 0)))

(defn inst->hex
  "Convert a 16-bit instruction integer to a 4-character uppercase hex string."
  [inst]
  #?(:clj  (format "%04X" (bit-and inst 0xFFFF))
     :cljs (.toUpperCase
             (.padStart (.toString (bit-and inst 0xFFFF) 16) 4 "0"))))

(defn hex->inst
  "Convert a 4-character hex string into a 16-bit instruction integer."
  [hex]
  (when-not (and (string? hex) (= 4 (count hex)))
    (throw (ex-info "Instruction hex must be exactly 4 characters"
                    {:hex hex})))
  #?(:clj
     (Integer/parseInt hex 16)

     :cljs
     (js/parseInt hex 16)))
