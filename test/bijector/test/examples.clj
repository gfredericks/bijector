(ns bijector.test.examples
  (:use bijector.core)
  (:use clojure.test bijector.test.helpers bijector.examples))

(deftest ascii-repetition-test
  (test-a-type (cartesian-product-type
                 (finite-nats (count simple-ascii-chars))                               
                 NATURALS
                 (lists-of (cartesian-product-type (finite-nats (dec (count simple-ascii-chars))) NATURALS))))
  (test-a-type ASCII-WITH-REPETITIONS))
