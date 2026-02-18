(ns regex-compiler.assembler
    "Code for assembling generated instructions into a final program"
    (:require [regex-compiler.instruction :as inst]))

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
