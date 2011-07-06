(ns bijector.test.core
  (:use [bijector.core])
  (:use [clojure.test]))

(defn- **
  [a b]
  (apply * (repeat b a)))

(def nz
  (concat
    (range 1 101)
    (for [n (range 1 51)] (* 1000 n))
    (for [n (range 1 51)] (** 578 n))))

(defn- test-a-type
  [t]
  (doseq [n nz]
    (is (= n (->> n (to t) (from t))))))

(defn- type-has-elements
  [t & els]
  (doseq [el els]
    (let [n (from t el)]
      (is (integer? n))
      (is (pos? n))
      (is (= el (to t n))))))

(deftest basic-types-test
  (doseq [t [NATURALS INTEGERS]]
    (test-a-type t)))

(deftest list-of-test
  (type-has-elements
    (list-of INTEGERS)
    [1 2 3]
    []
    [-1 -2 -1]
    [500007]))
