(ns nebula.lexer-test
  (:require [clojure.test :refer [deftest testing is]]
            [nebula.lexer :as sut]))

(deftest lexer-test
  (testing "when lexing strings"
    (testing "it captures the string"
      (let [input "\"foo bar 3.14\""
            lexed (sut/lex input)]
        (is (= [input] (mapv :text lexed)))))

    (testing "it captures strings across newlines"
      (let [input "\"foo \nbar 3.14\""
            lexed (sut/lex input)]
        (is (= [input] (mapv :text lexed)))))

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
               (mapv :text lexed)))))

    (testing "it throws on never-ending strings"
      (is (thrown-with-msg? Exception #"Unexpected EOF"
                            (sut/lex "\"some string"))))

    (testing "it deals with escaped quotation marks"
      (let [input "\"a \\\" string\""
            lexed (sut/lex input)]
        (is (= [input] (mapv :text lexed)))))

    (testing "it deals with escaped backslashes"
      (let [input "\"a \\\\ string\""
            lexed (sut/lex input)]
        (is (= [input] (mapv :text lexed)))))

    (testing "it deals with escaped newlines"
      (let [input "\"a \\n string\""
            lexed (sut/lex input)]
        (is (= [input] (mapv :text lexed)))))

    (testing "it throws if you try to escape something else"
      (is (thrown-with-msg? Exception #"Unsupported escape character"
                            (sut/lex "\"\\ \"")))))

  (testing "when lexing comments"
    (testing "it ignores the comment"
      (let [input "3.14;; comment"
            lexed (sut/lex input)]
        (is (= ["3.14"] (mapv :text lexed)))))

    (testing "it only ignores until it finds a newline"
      (let [input ";; comment\n3.14"
            lexed (sut/lex input)]
        (is (= ["3.14"] (mapv :text lexed)))))))
