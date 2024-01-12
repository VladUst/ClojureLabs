(ns lab4.core
  (:require [lab4.dnf :refer [dnf]]
            [lab4.expressions :refer :all]))

(defn -main []
  (println (dnf (|| (variable :A) (variable :B))))
  (println (dnf (&& (variable :A) (variable :B))))
  (println (dnf (no (&& (variable :A) (variable :B)))))
  (println (dnf (implication (variable :A) (variable :B))))
  (println (dnf (|| (variable :A) (|| (variable :A) (variable :B))))))