(ns nebula.parser)

(defn- float-kind? [token-text]
  (boolean (re-find #"^-?[0-9]*\.[0-9]+$" token-text)))

(defn- integer-kind? [token-text]
  (boolean (re-find #"^-?[0-9]+$" token-text)))

(defn- keyword-kind? [token-text]
  (boolean (re-find #"^:[a-zA-Z-_]+$" token-text)))

(defn- string-kind? [token-text]
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

(defn- parse-expression [x]
  ;; TODO do we actually need this?
  ;; TODO be smarter about this
  (read-string (:text x)))

(def pairs {:open-parenthesis :close-parenthesis
            :open-bracket :close-bracket
            :open-brace :close-brace})

(defn- parse-tokens
  ([tokens]
   (parse-tokens tokens [] {:enclosures (list)
                            :scopes (list)}))
  ([tokens acc state]
   (let [token (first tokens)
         token-kind (:token-kind token)]
     (cond
       (nil? token-kind)
       (do
         (when-not (empty? (:enclosures state))
           (throw (ex-info "Unexpected EOF" state)))
         acc)

       (-> token-kind #{:open-parenthesis
                        :open-bracket
                        :open-brace})
       (recur (rest tokens)
              []
              (-> state
                  (update :enclosures conj {:opening token
                                            :closing (pairs token-kind)
                                            :scope acc})))

       (-> token-kind #{:close-parenthesis
                        :close-bracket
                        :close-brace})
       (do
         (when (empty? (:enclosures state))
           (throw (ex-info (format "Unexpected %s" token-kind)
                           state)))
         (let [expected (-> state :enclosures peek :closing)]
           (when (not= expected token-kind)
             (throw (ex-info (format "Expected %s, got %s at %d:%d"
                                     expected token-kind (:line token) (:column token))
                             state))))
         (let [enclosure (peek (:enclosures state))]
           (recur (rest tokens)
                  (conj (:scope enclosure) {:scalar false
                                            :line (-> enclosure :opening :line)
                                            :column (-> enclosure :opening :column)
                                            :expression-kind :sexp
                                            :expression acc})
                  (-> state
                      (update :enclosures pop)))))

       :else ;; scalar values
       (recur (rest tokens)
              (as-> (first tokens) $
                (assoc $ :scalar true)
                (assoc $ :expression-kind (:token-kind $))
                (assoc $ :expression (parse-expression $))
                (dissoc $ :token-kind)
                (conj acc $))
              state)))))

(defn parse
  "`tokens` -> expressions."
  [tokens]
  (let [tokens-with-kinds (map attach-kind tokens)]
    (parse-tokens tokens-with-kinds)))
