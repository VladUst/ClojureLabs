(defn generate-permutations-for-word [word symbols]
  (when-let [char (first symbols)]
    (let [recur-result (generate-permutations-for-word word (rest symbols))]
      (cond
        (not= char (first word)) (conj recur-result (conj word char))
        :else recur-result))))

(defn get-words-permutations [words symbols]
  (when-let [first-word (first words)]
    (let [rest-words (rest words)
          generated-word (generate-permutations-for-word first-word symbols)
          generated-words (get-words-permutations rest-words symbols)]
      (concat generated-words generated-word))))


(defn generate-words [symbols n]
  (cond
    (> n 0) (get-words-permutations (generate-words symbols (dec n)) symbols)
    :else (list (list))))

(defn format-result [lists]
  (map #(apply str %) lists))

(println (format-result (generate-words (list "a" "b" "c") 2)))
(println (format-result (generate-words (list "a" "b" "c" "d") 2)))
(println (format-result (generate-words (list "a" "b" "c" "d") 3)))

















