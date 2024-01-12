(ns lab4.rules.utils
  (:require [lab4.expressions :refer :all]))

;Применение правил упрощения для выражения
(defn use-rules [expr rules]
  (some
   (fn [[condition result]]
     (when (condition expr)
       (result expr)))
   rules))

;Преобразование аргументов конъюнкции с помощью обработчика
(defn handle-conjuction-args [expr handler]
  (apply && (map handler (get-operation-args expr))))

;Преобразование аргументов дизъюнкции с помощью обработчика
(defn handle-disjunction-args [expr handler]
  (apply || (map handler (get-operation-args expr))))

;Преобразование аргумента отрицания с помощью обработчика
(defn handle-negation-args [expr handler]
  (let [arg (second expr)]
    (no (handler arg))))

;Избавление от отрицания с возвратом аргумента операции
(defn check-negation-variable-or-constant [arg]
  (if (or (variable? arg) (constant? arg)
          (and (no? arg)
               (or (variable? (first (get-operation-args arg)))
                   (constant? (first (get-operation-args arg))))))
    (if (or (variable? arg) (constant? arg))
      arg
      (first (get-operation-args arg)))))

;Преобразование тавтологий конъюнкции: !A ∧ A = False
(defn conjunction-tautologies-handler [expr]
  (let [args-list         (get-operation-args expr)
        no-negations-list (map #(check-negation-variable-or-constant %) args-list)]
    (cond
      (not= (count (distinct no-negations-list)) (count args-list))
      False

      :else
      expr)))

;Остаточное преобразование для случаев, если осталась конъюнкция с одним аргументом - ∧ A = A
(defn conjunction-single-handler [expr]
  (cond
    (and (&&? expr) (= 1 (count (get-operation-args expr))))
    (second expr)

    (&&? expr)
    expr

    :else
    expr))

;Финальное преобразование конъюнкции с избавлением от тавтологий и упрощением в случае остатка одного аргумента
(defn conjunction-final-handler [expr]
  (let [result (cond
                 (or (constant? expr) (variable? expr))
                 expr

                 (and (no? expr) (variable? (first (get-operation-args expr))))
                 expr

                 :else
                 (->> (get-operation-args expr)
                      distinct
                      (apply &&)
                      (conjunction-tautologies-handler)))]
    (conjunction-single-handler result)))

;Преобразование тавтологий дизъюнкции: !A ∨ A = True
(defn disjunction-tautologies-handler [expr]
  (let [args-list         (get-operation-args expr)
        no-negations-list (map #(if (no? %) (second %) %) args-list)]
    (cond
      (< (count (distinct no-negations-list)) (count (distinct args-list)))
      True

      :else
      (distinct expr))))

;Остаточное преобразование для случаев, если осталась дизъюнкция с одним аргументом - ∨ A = A
(defn disjunction-single-handler [expr]
  (cond
    (and (||? expr) (= 1 (count (get-operation-args expr))))
    (second expr)

    (||? expr)
    expr

    :else
    expr))

;Финальное преобразование дизъюнкции с избавлением от тавтологий и упрощением в случае остатка одного аргумента
(defn disjunction-final-handler [expr]
  (let [result (cond
                 (or (constant? expr) (variable? expr)
                     (and (no? expr) (variable? (first (get-operation-args expr)))))
                 expr

                 :else
                 (->> (get-operation-args expr)
                      (map conjunction-final-handler)
                      (apply ||)
                      (disjunction-tautologies-handler)))]
    (disjunction-single-handler result)))

;Финальное преобразование выражения с избавлением от тавтологий и упрощением в случае остатка одного аргумента
(defn tautologies-handler [expr]
  (let [result (cond
                 (or (constant? expr) (variable? expr) (no? expr)) expr
                 (&&? expr) (conjunction-final-handler expr)
                 (||? expr) (disjunction-final-handler expr))]
    result))
