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

(deftest basic-types-test
  (doseq [t [NATURALS INTEGERS]]
    (test-a-type t)))
