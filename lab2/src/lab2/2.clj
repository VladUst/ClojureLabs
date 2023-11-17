(ns lab2.2
  (:require [lab2.1 :refer :all]))

(defn calc-partial-sum [func h]
  (letfn [(area [cur-x]
            (calc-area func cur-x (+ cur-x h)))]
    (reductions +
                0
                (map area
                     (iterate (fn [x] (+ x h)) 0)))))

(defn calculate-integral-with-seq [function h]
  (let [partial-sum-seq (calc-partial-sum function h)]
    (fn [x]
      (nth partial-sum-seq (int (/ x h))))))






