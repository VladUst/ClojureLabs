(ns lab4.rules.constant-rules
  (:require [lab4.expressions :refer :all]
            [lab4.rules.utils :refer :all]))

;Применение правил для преобразования операций с константами
(declare use-constants-rules)

;Правила преобразования комплексных выражений с константами
;Конъюнкция - A ∧ True * expr = A ∧ expr, A ∧ Flase ∧ expr = False
;Дизъюнкция - A ∨ True ∨ expr = True,  A V False ∨ expr = A ∨ expr
(defn complex-constants-rules []
  (list
   [(fn [expr]
      (and (&&? expr)
           (some (fn [arg] (constant? arg)) (get-operation-args expr))))
    (fn [expr]
      (if (= (some (fn [arg] (when (constant? arg) arg)) (get-operation-args expr))
             False)
        False
        (apply &&
               (remove (fn [arg] (= arg True))
                       (get-operation-args expr)))))]
   [(fn [expr]
      (and (||? expr)
           (some (fn [arg] (constant? arg)) (get-operation-args expr))))
    (fn [expr]
      (if (= (some (fn [arg] (when (constant? arg) arg)) (get-operation-args expr))
             True)
        True
        (apply ||
               (map use-constants-rules
                    (remove (fn [arg] (= arg False))
                            (get-operation-args expr))))))]))

;Правила преобразований выражений с константами
(def binary-operations-with-constant-rules
  (concat
   (complex-constants-rules)
   (list
    ;Выполнение данного преобразования для всех аргументов операций конъюнкции, дизъюнкциЙ и отрицаний
    [(fn [expr] (&&? expr)) #(handle-conjuction-args % use-constants-rules)]
    [(fn [expr] (||? expr)) #(handle-disjunction-args % use-constants-rules)]
    ;Для переменных и констант, и отрицаний переменных преобразований не требуется
    [(fn [expr] (and (no? expr) (variable? (first (get-operation-args expr))))) identity]
    [(fn [expr] (variable? expr)) identity]
    [(fn [expr] (constant? expr)) identity])))

(defn use-constants-rules [expr]
  (use-rules expr binary-operations-with-constant-rules))