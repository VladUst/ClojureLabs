(ns lab4.expressions)

;Объявление переменных и констант
(def True (list :True))
(def False (list :False))
(defn variable [name]
  (list :var name))

;Проверка переменных и констант
(defn False? [expr]
  (= (first expr) :False))
(defn True? [expr]
  (= (first expr) :True))
(defn constant? [expr]
  (or (True? expr)
      (False? expr)))
(defn variable? [expr]
  (= (first expr) :var))

;Операция конъюнкции с проверкой
(defn && [expr & rest-args]
  (cons :&& (cons expr rest-args)))
(defn &&? [expr]
  (= (first expr) :&&))

;Операция дизъюнкции с проверкой
(defn || [expr & rest-args]
  (cons :|| (cons expr rest-args)))
(defn ||? [expr]
  (= (first expr) :||))

;Операция отрицания с проверкой
(defn no [expr & rest-args]
  (cons :no (cons expr rest-args)))
(defn no? [expr]
  (= (first expr) :no))

;Операция импликации с проверкой
(defn implication [expr & rest-args]
  (cons :implication (cons expr rest-args)))
(defn implication? [expr]
  (= (first expr) :implication))

;Получение аргументов операции
(defn get-operation-args [expr]
  (rest expr))