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

(deftest binary-partitions-type-test
  (doseq [parts (range 2 10)]
    (testing (str "Binary partitions of order " parts)
      (let [t (binary-partitions-type parts)]
        (test-a-type t)
        (type-has-elements t
          (repeat parts "")
          (repeat parts "0")
          (repeat parts "0110")
          (for [n (range 3898934 (+ 3898934 parts))]
            (.toString (bigint n) 2)))))))

(deftest finite-cartesian-product-test
  (testing "fixed-length boolean lists"
    (let [t (apply finite-cartesian-product-type (repeat 5 BOOLEANS))]
      (type-has-elements t
        [true true true true true]
        [false false false false false]
        [true false true false true]
        [true true false true true])
      (is (= 32 (cardinality t)))))
  (testing "ascii-boolean pairs"
    (let [t (finite-cartesian-product-type
              BOOLEANS
              (new EnumerationDataType (map str "ABCDEF")))]
      (is (= (cardinality t) 12))
      (type-has-elements t [true "C"] [false "D"] [true "A"] [true "F"]))))

(deftest infinite-cartesian-product-type-test
  (let [infinite-types [NATURALS
                        SIMPLE-ASCII
                        NESTED-NATURAL-LISTS
                        INTEGERS]]
    (testing "pairs of types"
      (for [t1 infinite-types, t2 infinite-types]
        (test-a-type (infinite-cartesian-product-type t1 t2))))
    (testing "triples of types"
      (for [t1 infinite-types, t2 infinite-types, t3 infinite-types]
        (test-a-type (infinite-cartesian-product-type t1 t2 t3))))))
