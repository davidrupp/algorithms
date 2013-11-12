(ns algorithms.core)

(defn gcd [a b]
  (if (= b 0)
    a
    (recur b (mod a b))))

(defn pow [n exp]
  (if (= 0 exp)
    1
    (*' n (pow n (dec exp)))))

(def fib-seq
  (iterate (fn [[x y]] [y (+' x y)]) [0 1]))

(def fib
  (map first fib-seq))
