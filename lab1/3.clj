(defn my-map [func coll]
  (reduce (fn [result elem]
            (cons (func elem) result))
          (list)
          (reverse coll)))

(defn my-filter [pred coll]
  (reduce (fn [result elem]
            (if (pred elem)
              (cons elem result)
              result))
          (list)
          (reverse coll)))

(println (my-map inc [1 2 3 4 5]))
(println (my-map (fn [x] (* x x)) [1 2 3 4 5]))

(println (my-filter even? [1 2 3 4 5 6 7 8 9 10]))