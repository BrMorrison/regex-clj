(ns regex-compiler.assembler
    "Code for assembling generated instructions into a final program"
    (:require [regex-compiler.instruction :as inst]
              [regex-compiler.util :as util]))

(defn- label-pass [[label-map pc] inst]
    (case (:op inst)
        :label [(assoc label-map (:label inst) pc) pc]
        [label-map (inc pc)]))

(defn- build-label-map [code]
    (first (reduce label-pass [{} 0] code)))

(defn- assemble-inst
    "Assembles an instruction, replacing labels with addresses"
    [label-map inst]
    (case (:op inst)
        (:char :match) inst
        :jmp   (inst/inst-jmp (get label-map (:dest inst)))
        :split (inst/inst-split (get label-map (:dest1 inst))
                                   (get label-map (:dest2 inst)))))

(defn assemble [code]
    (let [label-map (build-label-map code)
          filtered-code (remove #(= (:op %) :label) code)]
        (map (partial assemble-inst label-map) filtered-code)))

; Functions for encoding instructions to binary

(defn- encode-op [opcode] (bit-shift-left opcode 14))
(defn- encode-arg
    ([val pos] (bit-shift-left val pos))
    ([val pos mask] (encode-arg (bit-and val mask) pos)))

(defn- encode-match
    ;┌─────────┬──────────┐
    ;│ 15 - 14 │  13 - 0  │
    ;├─────────┼──────────┤
    ;│  0b11   │ reserved │
    ;└─────────┴──────────┘
    []
    (encode-op 2r11))

(defn- encode-char
    ;┌─────────┬──────────┬───────┐
    ;│ 15 - 14 │  13 - 8  │ 7 - 0 │
    ;├─────────┼──────────┼───────┤
    ;│  0b10   │ reserved │  char │
    ;└─────────┴──────────┴───────┘
    [inst]
    (bit-or (encode-op 2r10)
            (encode-arg (util/char->int (:char inst)) 0 16rff)))

(defn- encode-jump
    ;┌─────────┬──────────┬───────┐
    ;│ 15 - 14 │  13 - 7  │ 6 - 0 │
    ;├─────────┼──────────┼───────┤
    ;│  0b01   │ reserved │  dest │
    ;└─────────┴──────────┴───────┘
    [inst]
    (bit-or (encode-op 2r01)
            (encode-arg (:dest inst) 0 16r7f)))

(defn- encode-split
    ;┌─────────┬────────┬────────┐
    ;│ 15 - 14 │ 13 - 7 │  6 - 0 │
    ;├─────────┼────────┼────────┤
    ;│  0b00   │  dest1 │  dest2 │
    ;└─────────┴────────┴────────┘
    [inst]
    (bit-or (encode-op 2r00)
            (encode-arg (:dest1 inst) 7 16r7f)
            (encode-arg (:dest2 inst) 0 16r7f)))

(defn- encode-inst
    [inst]
    (case (:op inst)
        :match (encode-match)
        :char  (encode-char inst)
        :jmp   (encode-jump inst)
        :split (encode-split inst)))

(defn encode
    [prog]
    (mapv encode-inst prog))

(defn encode-str
    [prog]
    (->> prog
         encode
         (map util/inst->hex)
         (apply str)))