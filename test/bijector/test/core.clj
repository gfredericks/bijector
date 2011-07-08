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

(deftest union-types-test
  (doseq [t [(union-type NATURAL-LISTS SIMPLE-ASCII)
             (union-type (lists-of BOOLEANS) NATURALS)]]
    (test-a-type t))
  (let [t (union-type NATURAL-LISTS SIMPLE-ASCII)]
    (type-has-elements t [1 2 3] [] "" "[1 2 3]")))

(deftest recursive-types-test
  (test-a-type NESTED-NATURAL-LISTS)
  (type-has-elements NESTED-NATURAL-LISTS [] [[[]]] [[2 3 [4]] 5 6 []] (range 1 20)))

(deftest lists-of-test
  (type-has-elements
    (lists-of INTEGERS)
    [1 2 3]
    []
    [-1 -2 -1]
    [500007]
    (range 20)))
