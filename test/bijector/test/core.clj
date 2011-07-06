(ns bijector.test.core
  (:use [bijector.core])
  (:import (bijector.core EnumerationDataType))
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
    (testing (pr-str el)
      (let [n (from t el)]
        (is (integer? n))
        (is (pos? n))
        (is (= el (to t n)))))))

(deftest basic-types-test
  (doseq [t [NATURALS
             INTEGERS
             (lists-of BOOLEANS)
             NATURAL-LISTS
             SIMPLE-ASCII]]
    (test-a-type t)))

(deftest enumeration-type-test
  (testing "Binary"
    (let [t (new EnumerationDataType [0 1])]
      (doseq [x [1 2]]
        (is (= x (from t (to t x)))))))
  (testing "Ternary"
    (let [t (new EnumerationDataType [0 1 2])]
      (doseq [x [1 2 3]]
        (is (= x (from t (to t x))))))))


(deftest lists-of-test
  (type-has-elements
    (lists-of INTEGERS)
    [1 2 3]
    []
    [-1 -2 -1]
    [500007]))
