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

(defn- prngf 
  "Pseudo-random number generating function, as in Essential Algorithms, Chapter 2"
  [x]
  (let [a 7  ;; magic constants; TODO: do better
        b 5
        m 11]
    (-> x
        (* ,,, a)
        (+' ,,, b)
        (mod ,,, m))))

(def rnd
  (iterate prngf 0))
