(ns bijector.test.helpers
  (:use bijector.core)
  (:use clojure.test))

(defn- **
  [a b]
  (apply *' (repeat b a)))

(def nz
  (concat
    (range 1 101)
    (for [n (range 1 51)] (* 1000 n))
    (for [n (range 1 51)] (** 578 n))))

(defn test-a-type
  ([t] (test-a-type t nz))
  ([t nz]
    (doseq [n nz]
      (testing (str "with index " n)
        (let [v (to t n)]
          (is (= n (from t v)))
          (is (element? t v)))))))

(defn type-has-elements
  [t & els]
  (doseq [el els]
    (testing (pr-str el)
      (is (element? t el))
      (let [n (from t el)]
        (is (integer? n))
        (is (pos? n))
        (is (= el (to t n)))))))
