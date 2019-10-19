(ns nebula.lexer_test
  (:require [clojure.test :refer [deftest testing is]]
            [nebula.lexer :as sut]))

(deftest lexer-test
  (testing "when lexing strings"
    (testing "it captures the string"
      (let [input "\"foo bar 3.14\""
            lexed (sut/lex input)]
        (is (= 1 (count lexed)))
        (is (= input (:text (first lexed))))))

    (testing "it captures strings across newlines"
      (let [input "\"foo \nbar 3.14\""
            lexed (sut/lex input)]
        (is (= 1 (count lexed)))
        (is (= input (:text (first lexed))))))

    (testing "it keeps the string start"
      (let [input "\"foo \nbar 3.14\""
            lexed (sut/lex input)]
        (is (= 1 (-> lexed first :line)))
        (is (= 1 (-> lexed first :column)))))

    (testing "it keeps advancing the lexer state for following tokens"
      (let [input "\"foo \nbar 3.14\"\n 6.28"
            lexed (sut/lex input)]
        (is (= 3 (-> lexed last :line)))
        (is (= 2 (-> lexed last :column)))))

    (testing "it deals with tokens immediately before the string"
      (let [input "3.14\"string\""
            lexed (sut/lex input)]
        (is (= ["3.14" "\"string\""]
               (mapv :text lexed)))))

    (testing "it deals with tokens immediately after the string"
      (let [input "\"string\"3.14"
            lexed (sut/lex input)]
        (is (= ["\"string\"" "3.14"]
               (mapv :text lexed)))))))
