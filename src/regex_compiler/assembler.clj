(ns regex-compiler.assembler
    "Code for assembling generated instructions into a final program"
    (:require [regex-compiler.codegen :as codegen]))

(defn- label-pass [[label-map pc] inst]
    (case (:op inst)
        :label [(assoc label-map (:label inst) pc) pc]
        [label-map (inc pc)]))

(defn- build-label-map [code]
    (first (reduce label-pass [{} 0] code)))

(defn- assemble-inst [label-map inst]
    (case (:op inst)
        ; Omit labels in the final code
        :label ""
        :match "match\n" 
        :char  (format "char %c\n" (:char inst))
        :jmp   (format "jmp %d\n" (get label-map (:dest inst)))
        :split (format "split %d %d\n" 
                                     (get label-map (:dest1 inst))
                                     (get label-map (:dest2 inst)))))

(defn assemble [code]
    (let [label-map (build-label-map code)]
        (reduce str "" (map (partial assemble-inst label-map) code))))
