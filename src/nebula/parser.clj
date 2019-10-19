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

(defn- attach-kind [token]
  (let [text (:text token)]
    (assoc token :token-kind
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

(defn- parse-tokens
  ([tokens]
   (parse-tokens tokens [] {:level 0 :last-enclosing nil}))
  ([tokens acc state]
   (cond
     (empty? tokens)
     acc

     (-> tokens first :token-kind (= :open-parenthesis))
     (recur (rest tokens)
            acc
            (update state :level inc))

     (-> tokens first :token-kind (= :close-parenthesis))
     (recur (rest tokens)
            acc
            (update state :level dec))

     :else ;; scalar values
     (recur (rest tokens)
            (as-> (first tokens) $
              (assoc $ :scalar true)
              (assoc $ :expression-kind (:token-kind $))
              (dissoc $ :token-kind)
              (conj acc $))
            state))))

(defn parse
  "`tokens` -> expressions."
  [tokens]
  (let [tokens-with-kinds (map attach-kind tokens)]
    (parse-tokens tokens-with-kinds)))
