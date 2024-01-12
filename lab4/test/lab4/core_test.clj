(ns lab4.core-test
  (:require [clojure.test :refer :all]
            [lab4.dnf :refer [dnf]]
            [lab4.expressions :refer :all]))

(deftest negation-test
  (testing "!(A ∧ B) = !A V !B"
    (is
     (= (dnf (no (&& (variable :A) (variable :B))))
        (|| (no (variable :A)) (no (variable :B))))))
  (testing "!(A V B) = !A ∧ !B"
    (is
     (= (dnf (no (|| (variable :A) (variable :B))))
        (&& (no (variable :A)) (no (variable :B))))))
  (testing "!!A = A"
    (is
     (= (dnf (no (no (variable :A))))
        (variable :A))))
  (testing "!False = True"
    (is
     (= (dnf (no False))
        True)))
  (testing "!True = False"
    (is
     (= (dnf (no True))
        False))))

(deftest conjunction-test
  (testing "A ∧ B = B ∧ A"
    (is
     (= (dnf (&& (variable :A) (variable :B)))
        (&& (variable :A) (variable :B)))))
  (testing "!A ∧ A = False"
    (is
     (= (dnf (&& (no (variable :A)) (variable :A)))
        False)))
  (testing "A ∧ True = A"
    (is
     (= (dnf (&& (variable :A) True))
        (variable :A))))
  (testing "A ∧ False = False"
    (is
     (= (dnf (&& (variable :A) False))
        False))))

(deftest disjunction-test
  (testing "A V B = B V A"
    (is
     (= (dnf (|| (variable :A) (variable :B)))
        (|| (variable :A) (variable :B)))))
  (testing "!A V A = True"
    (is
     (= (dnf (|| (no (variable :A)) (variable :A)))
        True)))
  (testing "A V True = True"
    (is
     (= (dnf (|| (variable :A) True))
        True)))
  (testing "A V False = A"
    (is
     (= (dnf (|| (variable :A) False))
        (variable :A)))))

(deftest implication-test
  (testing "A -> A = True"
    (is
     (= (dnf (implication (variable :A) (variable :A)))
        True)))
  (testing "A -> B = !A V B"
    (is
     (= (dnf (implication (variable :A) (variable :B)))
        (|| (no (variable :A)) (variable :B)))))
  (testing "((A -> B) -> C = !A ∧ !B V C"
    (is
     (= (dnf (implication (|| (variable :A) (variable :B)) (variable :C)))
        (|| (&& (no (variable :A)) (no (variable :B))) (variable :C))))))

(deftest distributivity-test
  (testing "A ∧ (B V C) = (A ∧ B) V (A ∧ C)"
    (is
     (= (dnf (&& (variable :A) (|| (variable :B) (variable :C))))
        (|| (&& (variable :A) (variable :B))
            (&& (variable :A) (variable :C))))))
  (testing "(B V C) ∧ A = (B ∧ A) V (C ∧ A)"
    (is
     (= (dnf (&& (|| (variable :B) (variable :C)) (variable :A)))
        (|| (&& (variable :B) (variable :A))
            (&& (variable :C) (variable :A)))))))

(deftest complex-dnf-test
  (testing "A V (A V B) = A V B"
    (is (= (dnf (|| (variable :A) (|| (variable :A) (variable :B))))
           (|| (variable :A) (variable :B)))))
  (testing "A ∧ (!A ∧ (B ∧ C)) = False"
    (is (= (dnf (&& (variable :A) (&& (no (variable :A)) (&& (variable :B) (variable :C)))))
           False)))
  (testing "A V !(!(A->B) V C) V C ∧ A V !A = True"
    (is
     (= (dnf
         (||
          (variable :A)
          (no
           (||
            (no (implication (variable :A) (variable :B)))
            (variable :C)))
          (&& (variable :C) (variable :A))
          (no (variable :A))))
        True))))