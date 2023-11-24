(ns lab3.core-test
  (:require [clojure.test :refer :all]
            [lab3.core :refer :all]
            [lab3.1 :refer :all]
            [lab3.2 :refer :all]))

(deftest parallel-filter-test
  (testing "Test parallel filter"
    (let [test-data (take 20 (iterate inc -10))]
      (is (= (parallel-filter even? 3 test-data) (filter even? test-data)))
      (is (= (parallel-filter neg? 3 test-data) (filter neg? test-data)))
      (is (= (parallel-filter #(= % 0) 5 test-data) (filter #(= % 0) test-data)))
      (is (= (parallel-filter #(<= % 5) 2 test-data) (filter #(<= % 5) test-data)))
      (is (= (parallel-filter #(> % 2) 4 test-data) (filter #(> % 2) test-data)))
      (is (= (parallel-filter (fn [x] (<= 0 x 5)) 3 test-data) (filter (fn [x] (<= 0 x 5)) test-data)))
      (is (= (parallel-filter (fn [x] (= (mod x 3) 0)) 3 test-data) (filter (fn [x] (= (mod x 3) 0)) test-data)))
      (is (= (parallel-filter (fn [x] (pos? (* x x))) 2 test-data) (filter (fn [x] (pos? (* x x))) test-data))))))

(deftest lazy-filter-test
  (testing "Lazy parallel filter"
    (let [test-data (take 20 (iterate inc -10))]
      (is (= (lazy-parallel-filter neg? 2 3 test-data) (filter neg? test-data)))
      (is (= (lazy-parallel-filter #(= % 0) 3 5 test-data) (filter #(= % 0) test-data)))
      (is (= (lazy-parallel-filter #(<= % 5) 2 3 test-data) (filter #(<= % 5) test-data)))
      (is (= (lazy-parallel-filter (fn [x] (<= 0 x 5)) 3 3 test-data) (filter (fn [x] (<= 0 x 5)) test-data))))))

(deftest lazy-filter-infinite
  (testing "Lazy parallel filter with infinity sequence"
    (let [infinity-seq (iterate inc 0)]
      (is (= (take 10 (lazy-parallel-filter even? 5 6 infinity-seq)) (take 10 (filter even? infinity-seq))))
      (is (= (take 150 (lazy-parallel-filter even? 5 6 infinity-seq)) (take 150 (filter even? infinity-seq)))))))
