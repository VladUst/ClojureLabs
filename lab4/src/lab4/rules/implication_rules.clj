(ns lab4.rules.implication-rules
  (:require [lab4.expressions :refer :all]
            [lab4.rules.utils :refer :all]))

;Применение правил для преобразования импликаций
(declare use-implication-rules)

;Обработчик для импликации: преобразует A -> B к !A ∨ B 
(defn handle-implication [expr]
  (let [arg1 (first (get-operation-args expr))
        arg2 (second (get-operation-args expr))]
    (use-implication-rules (|| (no (use-implication-rules arg1))
                               (use-implication-rules arg2)))))

;Правила преобразования импликаций
(def implication-rules
  (list
   [(fn [expr] (implication? expr)) handle-implication]
   ;Выполнение данного преобразования для всех аргументов операций конъюнкции, дизъюнкциЙ и отрицаний
   [(fn [expr] (&&? expr)) #(handle-conjuction-args % use-implication-rules)]
   [(fn [expr] (||? expr)) #(handle-disjunction-args % use-implication-rules)]
   [(fn [expr] (no? expr)) #(handle-negation-args % use-implication-rules)]
   ;Для переменных и констант преобразование не требуется
   [(fn [expr] (variable? expr)) identity]
   [(fn [expr] (constant? expr)) identity]))

(defn use-implication-rules [expr]
  (use-rules expr implication-rules))



