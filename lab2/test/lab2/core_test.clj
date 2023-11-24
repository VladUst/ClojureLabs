(ns lab2.core-test
  (:require [clojure.test :refer :all]
            [lab2.1 :refer :all]
            [lab2.2 :refer :all]))

(defn is-equal
  [a b epsilon]
  (if (< (Math/abs (- a b)) epsilon)
    true
    false))

(deftest integral-test-mem
  (testing "Memoization"
    (let [integral-fn1 ((calculate-integral-with-memo (fn [_] 2) 0.01) 1)
          integral-fn2 ((calculate-integral-with-memo (fn [_] 2) 0.01) 2)
          integral-fn3 ((calculate-integral-with-memo (fn [x] x) 0.01) 1)
          integral-fn4 ((calculate-integral-with-memo (fn [x] x) 0.01) 2)
          integral-fn5 ((calculate-integral-with-seq (fn [x] (* 2 x)) 0.01) 3)]
      (is (= true (is-equal 2.0 integral-fn1 0.5)))
      (is (= true (is-equal 4.0 integral-fn2 0.5)))
      (is (= true (is-equal 0.5 integral-fn3 0.5)))
      (is (= true (is-equal 2.0 integral-fn4 0.5)))
      (is (= true (is-equal 9.0 integral-fn5 0.5))))))

(deftest integral-test-seq
  (testing "Infinity sequence"
    (let [integral-fn1 ((calculate-integral-with-seq (fn [x] (* 2 x)) 0.01) 5)
          integral-fn2 ((calculate-integral-with-seq (fn [x] (* 2 x)) 0.01) 3)
          integral-fn3 ((calculate-integral-with-seq (fn [x] x) 0.01) 1)
          integral-fn4 ((calculate-integral-with-seq (fn [x] x) 0.01) 5)
          integral-fn5 ((calculate-integral-with-seq (fn [_] 2) 0.01) 5)]
      (is (= true (is-equal 25.0 integral-fn1 0.5)))
      (is (= true (is-equal 9.0 integral-fn2 0.5)))
      (is (= true (is-equal 0.5 integral-fn3 0.5)))
      (is (= true (is-equal 12.5 integral-fn4 0.5)))
      (is (= true (is-equal 10.0 integral-fn5 0.5))))))

