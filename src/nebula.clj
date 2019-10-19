(ns nebula
  (:require [nebula.lexer :refer [lex]]
            [nebula.parser :refer [parse]]
            [nebula.types :refer [infer-types]]))

(defn build-ast [source]
  (-> source
      lex
      parse
      infer-types))
