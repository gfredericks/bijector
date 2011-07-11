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
