(ns bijector.test.examples
  (:use bijector.core)
  (:use clojure.test bijector.test.helpers bijector.examples))

(deftest ascii-repetition-test
  (test-a-type (cartesian-product-type
                 (finite-nats (count simple-ascii-chars))                               
                 NATURALS
                 (lists-of (cartesian-product-type (finite-nats (dec (count simple-ascii-chars))) NATURALS))))
  (test-a-type ASCII-WITH-REPETITIONS (range 1 500)))

(deftest json-maps-test
  (test-a-type (json-maps INTEGERS)))

(deftest simple-json-test
  (test-a-type SIMPLE-JSON)
  (type-has-elements SIMPLE-JSON
    false
    true
    nil
    {"wut" "bagger"}
    [[[{"hey" [1 2 3]} false true [nil nil nil]]]]))

(deftest rationals-test
  (test-a-type POSITIVE-RATIONALS)
  (test-a-type RATIONALS)
  (type-has-elements RATIONALS 0 -1 -1/2 3 3/4 -7/6))

(deftest permutations-test
  (let [t (natural-permutations 3)]
    (type-has-elements t [0 1 2] [2 1 0] [1 2 0] [0 2 1] [1 0 2] [2 0 1]))
  (doseq [n [4 5 6]]
    (let [t (natural-permutations n),
          fact (apply * (range 1 (inc n)))]
      (test-a-type t (range 1 (inc fact))))))

(deftest non-repeating-natural-lists-test
  (let [t (non-repeating-natural-lists 5)]
    (test-a-type t)))

(deftest min-length-lists-of-test
  (test-a-type (min-length-lists-of NATURALS 5)))

(deftest money-csv-test
  (test-a-type (money-csv 5 10 20 50 5000)))
