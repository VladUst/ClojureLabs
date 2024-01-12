(ns lab4.dnf
  (:require [lab4.rules.constant-rules :refer [use-constants-rules]]
            [lab4.rules.distribution-rules :refer [use-distributivity-rules]]
            [lab4.rules.implication-rules :refer [use-implication-rules]]
            [lab4.rules.inner-rules :refer [use-inner-expressions-rules]]
            [lab4.rules.negation-rules :refer [use-negation-rules]]
            [lab4.rules.utils :refer [tautologies-handler]]))

;Преобразование к ДНФ
;1. Избавление от сложных операций (в частности, импликации) с приведением к конъюнкции, дизъюнкции, отрицанию
;2. Применение законов Де Моргана и избавление от двойных отрицаний
;3. Применение законов дистрибутивности
;4. Избавление от лишних скобок и вложенностей
;5. Избавление от лишних констант
;6. Обработка тавтологий
(defn dnf [expression]
  (->>
   expression
   use-implication-rules
   use-negation-rules
   use-distributivity-rules
   use-inner-expressions-rules
   use-constants-rules
   tautologies-handler))