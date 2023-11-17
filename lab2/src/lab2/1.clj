(ns lab2.1)

(defn calc-area [f x y]
  (let [h (- y x)]
    (* (/ (+ (f x) (f y)) 2) h)))

(defn integrate-segment [func h x]
  (let [y (+ x h)]
    (calc-area func x y)))

(defn integrate [func h x]
  (if (> x 0)
    (+ (integrate-segment func h x)
       (integrate func h (- x h)))
    0))

(defn calculate-integral-with-memo [func h]
  (let [memoized-integrate (memoize (fn [x] (integrate func h x)))]
    memoized-integrate))