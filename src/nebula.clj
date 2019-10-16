(ns nebula)

(def ^:private whitespace? #{\space \tab})
(def ^:private newline? #{\newline})
(def ^:private token-boundary? #{\( \) \[ \] \{ \}})

(defn- make-token [chars lexer-state]
  {:text (apply str chars)
   :line (:line lexer-state)
   :column (:column lexer-state)})

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

(defn lex
  "String -> tokens."
  ([source]
   (lex (seq source) [] {:line 1
                         :column 1}))
  ([source acc state]
   (if (empty? source)
     acc
     (let [[token tail new-state] (advance source state)]
       (recur tail
              (conj acc token)
              new-state)))))
