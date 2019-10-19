(ns nebula
  (:require [clojure.pprint :refer [pprint]]
            [nebula.lexer :refer [lex]]
            [nebula.parser :refer [parse]]
            [nebula.types :refer [infer-types]]))

(defn build-ast [source]
  (-> source
      lex
      parse
      #_infer-types))

(defn repl []
  (let [input (read-line)]
    (when (not= input "\\q")
      (println input)
      (pprint (build-ast input))
      (recur))))
