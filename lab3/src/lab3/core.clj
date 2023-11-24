(ns lab3.core
  (:gen-class)
  (:require [lab3.1 :refer :all]
            [lab3.2 :refer :all]))

(defn heavy-even? [x] (do (Thread/sleep 5) (even? x)))

(defn -main []
  (println "Common filter")
  (time (doall (filter heavy-even? (take 20 (iterate inc -10)))))
  (println "Parallel filter")
  (time (doall (parallel-filter heavy-even? 2 (take 20 (iterate inc -10)))))
  (println "Lazy filter")
  (time (doall (lazy-parallel-filter heavy-even? 4 3 (take 20 (iterate inc -10))))))
