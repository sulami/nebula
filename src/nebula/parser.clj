(ns nebula.parser)

(defn- float-kind? [token-text]
  (boolean (re-find #"^-?[0-9]*\.[0-9]+$" token-text)))

(defn- integer-kind? [token-text]
  (boolean (re-find #"^-?[0-9]+$" token-text)))

(defn- keyword-kind? [token-text]
  (boolean (re-find #"^:[a-zA-Z-_]+$" token-text)))

(defn- string-kind? [token-text] ;; TODO special characters & escape sequences
  (boolean (re-find #"^\".*\"$" token-text)))

(defn- symbol-kind? [token-text]
  (boolean (re-find #"^[a-zA-Z-_<>!*?]+$" token-text)))

(defn- parse-token [token]
  (let [text (:text token)]
    (assoc token :kind
           (cond
             (= "(" text)
             :open-parenthesis
             (= ")" text)
             :close-parenthesis

             (= "[" text)
             :open-bracket
             (= "]" text)
             :close-bracket

             (= "{" text)
             :open-brace
             (= "}" text)
             :close-brace

             (or (= "true" text)
                 (= "false" text))
             :boolean

             (= "nil" text)
             :nil

             (float-kind? text)
             :float

             (integer-kind? text)
             :integer

             (string-kind? text)
             :string

             (keyword-kind? text)
             :keyword

             (symbol-kind? text)
             :symbol

             :else
             :error))))

(defn parse
  "Attaches the `:kind` to `tokens`."
  [tokens]
  (map parse-token tokens))
