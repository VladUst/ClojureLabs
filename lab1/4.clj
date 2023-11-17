(defn get-words-permutations [words symbols]
  (letfn [(generate-permutations-for-word [word]
            (reduce (fn [acc char]
                      (cons (concat word (list char)) acc))
                    (list)
                    (filter (fn [char] (not= char (first word))) symbols)))]
    (reduce (fn [acc word]
              (concat (generate-permutations-for-word word) acc))
            (list)
            words)))

(defn generate-words [symbols n]
  (nth (iterate (fn [permutations] (get-words-permutations permutations symbols)) (list (list))) n))

(defn format-result [lists]
  (map #(apply str %) lists))

(println (format-result (generate-words (list "a" "b" "c") 2)))
(println (format-result (generate-words (list "a" "b" "c" "d") 2)))
(println (format-result (generate-words (list "a" "b" "c" "d") 3)))








