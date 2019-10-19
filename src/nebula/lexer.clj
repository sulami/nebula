(ns nebula.lexer)

(def ^:private whitespace? #{\space \tab})
(def ^:private newline? #{\newline})
(def ^:private comment? #{\;})
(def ^:private quotation-mark? #{\"})
(def ^:private token-boundary? #{\( \) \[ \] \{ \}})

(defn- make-token [chars lexer-state]
  {:text (apply str chars)
   :line (:line lexer-state)
   :column (:column lexer-state)})

(defn- lex-string
  "Lexes a string token.

  Strings can span several lines, so we need to keep track of where it
  started while advancing the state. There's also some special
  escaping handling going on within the string. On the bright side we
  mostly ignore string contents otherwise."
  ([source state]
   (lex-string source state state [\"]))
  ([source state start acc]
   (if (quotation-mark? (first source))
     [(make-token (conj acc (first source)) start)
      (rest source)
      (-> state
          (update :column inc))]
     (recur (rest source)
            (if (newline? (first source))
              (-> state
                  (update :line inc)
                  (assoc :column 1))
              (update state :column inc))
            start
            (conj acc (first source))))))

(defn- advance
  "Walks the lexer by one char and returns a token & updated source
  and state."
  ([source state] (advance source state []))
  ([source state acc]
   (cond
     (empty? source)
     [(make-token acc state)
      source
      state]

     (:comment state)
     (recur (rest source)
            (if (newline? (first source))
              (-> state
                  (assoc :comment false)
                  (update :line inc)
                  (assoc :column 1))
              (update state :column inc))
            acc)

     (comment? (first source))
     (if (empty? acc)
       (recur (rest source)
              (-> state
                  (assoc :comment true)
                  (update :column inc))
              acc)
       [(make-token acc state)
        source
        (assoc state :comment true)])

     (quotation-mark? (first source))
     (if (empty? acc)
       (lex-string (rest source) state)
       [(make-token acc state)
        source
        state])

     (whitespace? (first source))
     (if (empty? acc)
       (recur (rest source)
              (update state :column inc)
              acc)
       [(make-token acc state)
        source
        (update state :column #(+ % (count acc)))])

     (newline? (first source))
     (if (empty? acc)
       (recur (rest source)
              (-> state
                  (update :line inc)
                  (assoc :column 1))
              acc)
       [(make-token acc state)
        source
        (update state :column #(+ % (count acc)))])

     (token-boundary? (first source))
     (if (seq acc)
       [(make-token acc state)
        source
        (update state :column #(+ % (count acc)))]
       [(make-token (take 1 source) state)
        (rest source)
        (update state :column inc)])

     :else
     (recur (rest source)
            state
            (conj acc (first source))))))

;; TODO empty source produces one empty token

(defn lex
  "String -> tokens."
  ([source]
   (lex (seq source) [] {:line 1
                         :column 1
                         :comment false}))
  ([source acc state]
   (if (empty? source)
     acc
     (let [[token tail new-state] (advance source state)]
       (recur tail
              (conj acc token)
              new-state)))))
