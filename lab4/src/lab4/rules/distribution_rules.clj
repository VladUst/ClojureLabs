(ns lab4.rules.distribution-rules
  (:require [lab4.expressions :refer :all]
            [lab4.rules.utils :refer :all]))

;Применение правил для преобразования выражений по законам дистрибутивности
(declare use-distributivity-rules)

;Законы дистрибутивности для преобразования выражений - A ∧ (B ∨ C) = (A ∧ B) ∨ (A ∧ C), (A ∨ B) ∧ C = (A ∧ C) ∨ (B ∧ C)
(defn distributivity-laws []
  (list
   [(fn [expr] (and (&&? expr) (||? (nth expr 2))))
    (fn [expr]
      (use-distributivity-rules
       (||
        (&&
         (first (get-operation-args expr))
         (first (get-operation-args (second (get-operation-args expr)))))
        (&&
         (first (get-operation-args expr))
         (second (get-operation-args (second (get-operation-args expr))))))))]
   [(fn [expr] (and (&&? expr) (||? (second expr))))
    (fn [expr]
      (use-distributivity-rules
       (||
        (&& (first (get-operation-args (first (get-operation-args expr))))
            (second (get-operation-args expr)))
        (&& (second (get-operation-args (first (get-operation-args expr))))
            (second (get-operation-args expr))))))]))

;Правила преобразований в соотвтетствии с законами дистрибутивности
(def distributivity-rules
  (concat
   (distributivity-laws)
   (list
    ;Выполнение данного преобразования для всех аргументов операций конъюнкции, дизъюнкциЙ и отрицаний
    [(fn [expr] (&&? expr)) #(handle-conjuction-args % use-distributivity-rules)]
    [(fn [expr] (||? expr)) #(handle-disjunction-args % use-distributivity-rules)]
    [(fn [expr] (no? expr)) #(handle-negation-args % use-distributivity-rules)]
    ;Для переменных и констант преобразование не требуется
    [(fn [expr] (variable? expr)) identity]
    [(fn [expr] (constant? expr)) identity])))

(defn use-distributivity-rules [expr]
  (use-rules expr distributivity-rules))