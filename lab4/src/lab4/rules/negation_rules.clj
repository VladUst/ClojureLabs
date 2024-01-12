(ns lab4.rules.negation-rules
  (:require [lab4.expressions :refer :all]
            [lab4.rules.utils :refer :all]))

;Применение правил для преобразования отрицаний
(declare use-negation-rules)

;Законы Де Моргана для преобразования отрицаний конъюнкций и дизъюнкций
(defn de-morgan-laws []
  (list
   [(fn [expr] (and (no? expr) (&&? (second expr))))
    (fn [expr]
      (use-negation-rules
       (apply ||
              (map (fn [arg] (no arg))
                   (get-operation-args (second expr))))))]
   [(fn [expr] (and (no? expr) (||? (second expr))))
    (fn [expr]
      (use-negation-rules
       (apply &&
              (map (fn [arg] (no arg))
                   (get-operation-args (second expr))))))]))

;Раскрытие двойного отрицания, приведение True к False и False к True для констант
(defn common-negation-rules []
  (list
   [(fn [expr] (and (no? expr) (no? (second expr))))
    (fn [expr] (use-negation-rules (first (get-operation-args (second expr)))))]
   [(fn [expr] (and (no? expr) (True? (first (get-operation-args expr)))))
    (fn [_] False)]
   [(fn [expr] (and (no? expr) (False? (first (get-operation-args expr)))))
    (fn [_] True)]))

;Правила преобразования отрицаний
(def negation-rules
  (concat
   (de-morgan-laws)
   (common-negation-rules)
   (list
    ;Выполнение данного преобразования для всех аргументов операций конъюнкции, дизъюнкциЙ и отрицаний
    [(fn [expr] (&&? expr)) #(handle-conjuction-args % use-negation-rules)]
    [(fn [expr] (||? expr)) #(handle-disjunction-args % use-negation-rules)]
    [(fn [expr] (no? expr)) #(handle-negation-args % use-negation-rules)]
    ;Для переменных и констант преобразование не требуется
    [(fn [expr] (variable? expr)) identity]
    [(fn [expr] (constant? expr)) identity])))

(defn use-negation-rules [expr]
  (use-rules expr negation-rules))
