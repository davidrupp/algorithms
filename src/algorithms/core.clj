(ns algorithms.core
  (:refer-clojure :exclude [* - + == /])
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators))

(defn gcd [a b]
  (if (= b 0)
    a
    (recur b (mod a b))))

(defn pow1 [n exp]
  "naive version O(n)"
  (if (= 0 exp)
    1
    (*' n (pow1 n (dec exp)))))

(defn- nexp [exp]
  (if (even? exp)
    (/ exp 2)
    (/ (dec exp) 2)))

(defn pow2 [n exp]
  "better version O(lg n)"
  (if (= 0 exp)
    1
    (let [mem (pow2 n (nexp exp))]
      (*' mem mem (if (even? exp) 1 n)))))

(defn mpow [m exp]
  (let [ident (identity-matrix 2)]
    (if (= 0 exp)
      ident
      (let [mem (mpow m (nexp exp))]
        (mmul mem mem (if (even? exp) ident m))))))

(def fib-seq
  (iterate (fn [[x y]] [y (+' x y)]) [0 1]))

(def fib
  (map first fib-seq))

(def mfib-seq
  (let [z [[0 0] [0 0]]
        f (matrix [[1 1] [1 0]])]
    (lazy-seq (cons z (iterate (partial mmul f) f)))))

(def mfib
  (map #(second (first %)) mfib-seq))

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
