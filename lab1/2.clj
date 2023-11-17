(defn generate-permutations-for-word [word symbols]
  (loop [remaining-symbols symbols
         result (list)]
    (if (not-empty remaining-symbols)
      (let [char (first remaining-symbols)
            cur-result (if (not= char (first word))
                         (cons (cons char word) result)
                         result)]
        (recur (rest remaining-symbols) cur-result))
      result)))

(defn get-words-permutations [words symbols]
  (loop [remaining-words words
         result (list)]
    (if (not-empty remaining-words)
      (recur (rest remaining-words)
             (concat result (generate-permutations-for-word (first remaining-words) symbols)))
      result)))

(defn generate-words [symbols n]
  (loop [remaining-n n
         result (list (list))]
    (cond
      (> remaining-n 0) (recur (dec remaining-n) (get-words-permutations result symbols))
      :else result)))

(defn format-result [lists]
  (map #(apply str %) lists))

(println (format-result (generate-words (list "a" "b" "c") 2)))
(println (format-result (generate-words (list "a" "b" "c" "d") 2)))
(println (format-result (generate-words (list "a" "b" "c" "d") 3)))





