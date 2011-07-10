(ns bijector.test.core
  (:use [bijector.core])
  (:import (bijector.core EnumerationDataType))
  (:use [clojure.test]
        bijector.test.helpers))

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

(deftest natural-sets-test
  (test-a-type NATURAL-SETS)
  (type-has-elements NATURAL-SETS #{} #{1 2} #{1 2 3 4} #{8382497} #{4839294 1}))

(deftest sets-of-test
  (doseq [t [SIMPLE-ASCII INTEGERS NATURALS NESTED-NATURAL-LISTS]]
    (test-a-type (sets-of t))
    (type-has-elements (sets-of t)
      #{}
      #{(to t 2) (to t 4823)}
      #{(to t 928442) (to t 392472742948234)})))

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
      (doseq [t1 infinite-types, t2 infinite-types]
        (test-a-type (infinite-cartesian-product-type t1 t2) (range 1 100))))
    (testing "triples of types"
      (doseq [t1 infinite-types, t2 infinite-types, t3 infinite-types]
        (test-a-type (infinite-cartesian-product-type t1 t2 t3) (range 1 100))))))

(deftest cartesian-product-type-test
  (let [infinite-types [NATURALS
                        SIMPLE-ASCII
                        NESTED-NATURAL-LISTS],
        finite-types   [BOOLEANS
                        (new EnumerationDataType "gary")]]
    (testing "pairs of types"
      (doseq [t1 infinite-types, t2 finite-types]
        (test-a-type (cartesian-product-type t1 t2) (range 1 100))
        (test-a-type (cartesian-product-type t2 t1) (range 1 100))))
    (testing "triples of types"
      (doseq [t1 infinite-types, t2 finite-types, t3 infinite-types]
        (test-a-type (cartesian-product-type t1 t2 t3) (range 1 100))
        (test-a-type (cartesian-product-type t1 t3 t2) (range 1 100))
        (test-a-type (cartesian-product-type t2 t3 t1) (range 1 100))
        (test-a-type (cartesian-product-type t3 t2 t1) (range 1 100))))))
