(ns lab3.2
  (:require [lab3.1 :refer :all]))

(defn lazy-parallel-filter [pred block-size threads coll]
  (flatten
   (map
    (fn [blocks]
      (map deref
           (doall
            (map
             (fn [block] (process-block pred block))
             (partition block-size blocks)))))
    (partition (* block-size threads) coll))))


