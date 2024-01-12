(ns lab4.rules.inner-rules
  (:require [lab4.expressions :refer :all]
            [lab4.rules.utils :refer :all]))

;Применение правил для раскрытия скобок вложенных выражений
(declare use-inner-expressions-rules)

;Правила раскрытия скобок вложенных конъюнкций/дизъюнкций - A ∨ (B ∨ C) = A ∨ B ∨ C, (A ∧ B) ∧ C = A ∧ B ∧ C
(defn remove-brackets-rules []
  (list
   [(fn [expr]
      (and (&&? expr)
           (some (fn [arg] (&&? arg)) (get-operation-args expr))))
    (fn [expr]
      (let [conj      (some (fn [arg] (if (&&? arg) arg nil)) (get-operation-args expr))
            conj-args (get-operation-args conj)]
        (use-inner-expressions-rules
         (apply &&
                (concat (remove (fn [arg] (= arg conj)) (get-operation-args expr))
                        conj-args)))))]
   [(fn [expr]
      (and (||? expr)
           (some (fn [arg] (||? arg)) (get-operation-args expr))))
    (fn [expr]
      (let [disj      (some (fn [arg] (if (||? arg) arg nil)) (get-operation-args expr))
            disj-args (get-operation-args disj)]
        (use-inner-expressions-rules
         (apply ||
                (concat (remove (fn [arg] (= arg disj)) (get-operation-args expr)) disj-args)))))]))


;Правила для раскрытия скобок
(def inner-expressions-rules
  (concat
   (remove-brackets-rules)
   (list
    ;Выполнение данного преобразования для всех аргументов операций конъюнкции, дизъюнкциЙ и отрицаний
    [(fn [expr] (&&? expr)) #(handle-conjuction-args % use-inner-expressions-rules)]
    [(fn [expr] (||? expr)) #(handle-disjunction-args % use-inner-expressions-rules)]
    [(fn [expr] (no? expr)) #(handle-negation-args % use-inner-expressions-rules)]
    ;Для переменных и констант преобразование не требуется
    [(fn [expr] (variable? expr)) identity]
    [(fn [expr] (constant? expr)) identity])))


(defn use-inner-expressions-rules [expr]
  (use-rules expr inner-expressions-rules))