(ns regex-cli.core
    (:gen-class)
    (:require [regex-compiler.core :as compiler]
              [regex-vm.vm :as vm]))

(defn usage []
    (println "Usage:")
    (println "  regex-cli <regex> <input-string>")
    (println "")
    (println "Example:")
    (println "  regex-cli \"(a|b)*c\" \"aabac\""))

(defn -main
    [& args]
    (if (not (= (count args) 2))
        (usage)
        (let [[regex-str input-str] args
              prog (compiler/compile-regex regex-str)]
            (if (vm/match? prog input-str)
                (println "Match!")
                (println "No Match!")))))
