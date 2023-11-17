(ns lab2.core
  (:gen-class)
  (:require [lab2.1 :refer :all]
            [lab2.2 :refer :all]))

(defn func [x]
  (* 2 x))

(defn -main []
  (println "Memoization")
  (let [memoized-integrate (calculate-integral-with-memo func 0.5)]
    (time (memoized-integrate 100))
    (time (memoized-integrate 100)))
  (println "Infinity sequence")
  (let [seq-integrate (calculate-integral-with-seq func 0.5)]
    (time (seq-integrate 100))
    (time (seq-integrate 100))))

