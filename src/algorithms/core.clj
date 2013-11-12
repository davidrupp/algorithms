(ns algorithms.core)

(defn gcd [a b]
  (if (= b 0)
    a
    (recur b (mod a b))))

(defn pow [n exp]
  (if (= 0 exp)
    1
    (*' n (pow n (dec exp)))))
