(ns nebula
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [nebula.lexer :refer [lex]]
            [nebula.parser :refer [parse]]
            [nebula.types :refer [infer-types]]))

(defn- token-kind->char [tk]
  (tk {:close-parenthesis \)
       :close-bracket \]
       :close-brace \}}))

(defn build-ast [source]
  (try
    (-> source
        lex
        parse
        #_infer-types)
    (catch Exception e
      (case (:error (ex-data e))

        :unexpected-token
        (let [error-token (-> e ex-data :token)]
          (println (format
                    "Unexpected token \"%s\" at line %d, column %d:

  %s
  %s"
                    (:text error-token)
                    (:line error-token)
                    (:column error-token)
                    (nth (str/split-lines source) (-> error-token :line dec))
                    (str (apply str (repeat (-> error-token :column dec) \space))
                         (apply str (repeat (-> error-token :text count) \^))))))

        :wrong-token
        (let [error-token (-> e ex-data :token)]
          (println (format
                    "Expected \"%s\", got \"%s\" at line %d, column %d:

  %s
  %s"
                    (-> e ex-data :expected token-kind->char)
                    (:text error-token)
                    (:line error-token)
                    (:column error-token)
                    (nth (str/split-lines source) (-> error-token :line dec))
                    (str (apply str (repeat (-> error-token :column dec) \space))
                         (apply str (repeat (-> error-token :text count) \^))))))

        :unexpected-eof
        (println "Unexpected EOF")

        :unsupported-escape-char
        (println (format
                  "Unsupported escape character \"\\%s\" at line %d, column %d:

  %s
  %s"
                  (-> e ex-data :char)
                  (-> e ex-data :state :line)
                  (-> e ex-data :state :column)
                  (nth (str/split-lines source) (-> e ex-data :state :line dec))
                  (str (apply str (repeat (-> e ex-data :state :column dec) \space))
                       "^^")
                  ))

        (throw e)))))

(defn repl []
  (let [input (read-line)]
    (when (not= input "\\q")
      (println input)
      (pprint (build-ast input))
      (recur))))
