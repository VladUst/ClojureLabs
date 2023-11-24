(ns lab2.1)

(defn calc-area [f x y h]
  (* (/ (+ (f x) (f y)) 2) h))

(defn integrate-with-memo [f h]
  (memoize (fn [n]
             (let [x (* (dec n) h)
                   y (* n h)]
               (if (> n 0)
                 (+
                  ((integrate-with-memo f h) (dec n))
                  (calc-area f x y (- y x)))
                 0)))))

(defn calculate-integral-with-memo [func h]
  (let [memoized-integrate (integrate-with-memo func h)]
    (fn [x] (let [n-parts (int (/ x h))
                  end-point-after-discretization (* h n-parts)
                  rest-area (calc-area func x end-point-after-discretization (- x end-point-after-discretization))]
              (+
               (memoized-integrate n-parts)
               rest-area)))))