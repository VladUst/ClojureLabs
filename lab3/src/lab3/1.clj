(ns lab3.1)

(defn process-block [pred block]
  (future (doall (filter pred block))))

(defn split-coll [coll block-size]
  (loop [remaining coll
         result []]
    (if (empty? remaining)
      result
      (recur (drop block-size remaining)
             (conj result (take block-size remaining))))))

(defn parallel-filter [pred block-size coll]
  (apply concat
         (map deref
              (doall
               (map (fn [block] (process-block pred block))
                    (doall
                     (split-coll coll block-size)))))))

