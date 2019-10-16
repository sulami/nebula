(ns nebula
  (:require [nebula.lexer :refer [lex]]
            [nebula.parser :refer [parse]]))

(defn build-ast [source]
  (-> source
      lex
      parse))
