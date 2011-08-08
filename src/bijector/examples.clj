(ns bijector.examples
  (:require [clojure.string :as string])
  (:import (bijector.core DataType InfiniteDataType EnumerationDataType))
  (:use bijector.core))

(defn finite-nats
  [max-value]
  (integer-range-type 1 (inc max-value)))

; Careful converting large random numbers, as the strings could be
; exponentially long
(def ASCII-WITH-REPETITIONS
  (let [decd-nats (finite-nats (dec (count simple-ascii-chars))),
        raw-type (cartesian-product-type
                   (finite-nats (count simple-ascii-chars))
                   NATURALS
                   (lists-of (cartesian-product-type decd-nats NATURALS))),
        to-char (fn [n] (.substring simple-ascii-chars (dec n) n)),
        from-char
          (memoize (fn [c] (inc (.indexOf simple-ascii-chars (str c)))))]
    (wrap-type raw-type
      (fn [[first-char-index first-char-count pairs]]
        (apply str
          (flatten
            (list
              (repeat first-char-count (to-char first-char-index))
              (first
                (reduce
                  (fn [[s-so-far last-char-index] [new-index count]]
                    (let [altered-new-index
                           (if (>= new-index last-char-index) (inc new-index) new-index)]
                      [(apply str s-so-far (repeat count (to-char altered-new-index)))
                       altered-new-index]))
                  ["" first-char-index]
                  pairs))))))
      (fn [s]
        (loop [last-index nil, pairs [], s s]
          (if (empty? s)
            (concat (first pairs) [(rest pairs)])
            (let [[same diff] (split-with #{(first s)} s),
                  this-index (from-char (first s)),
                  adj-index (if (and last-index (> this-index last-index))
                              (dec this-index)
                              this-index)]
              (recur
                this-index
                (conj pairs [adj-index (count same)])
                diff)))))
      (fn [s] (and (element? SIMPLE-ASCII s) (not (empty? s)))))))

(defn nonempty-json-maps
  [value-t]
  (let [key-type (without (sets-of SIMPLE-ASCII) #{}),
        val-type (memoize #(tuples-of % value-t))]
    (wrap-type
      (pairs-of NATURALS)
      (fn [[a b]]
        (let [key-set (to key-type a),
              value-list (to (val-type (count key-set)) b)]
          (zipmap (sort key-set) value-list)))
      (fn [m]
        (let [left-n (from key-type (set (keys m))),
              right-n (from (val-type (count m)) (map val (sort-by key m)))]
          [left-n right-n]))
      (fn [m]
        (and (map? m) (every? #(and (string? (key %)) (element? value-t (val %))) m))))))

(defn json-maps
  [value-t]
  (union-type (new EnumerationDataType [{}]) (nonempty-json-maps value-t)))

(declare SIMPLE-JSON)

(def simple-json-stub
  (stub-type (fn [] SIMPLE-JSON)))

(def lists-of-simple-json (lists-of simple-json-stub))

(def vectors-of-simple-json
  (wrap-type lists-of-simple-json
    vec
    identity
    #(and (vector? %) (element? lists-of-simple-json %))))

(def maps-of-simple-json (json-maps simple-json-stub))

(def SIMPLE-JSON
  (union-type
    (new EnumerationDataType [true false nil])
    INTEGERS
    SIMPLE-ASCII
    vectors-of-simple-json
    maps-of-simple-json))

(defn ratio
  ([a b] (new clojure.lang.Ratio (bigint a) (bigint b)))
  ([x]
    (cond (ratio? x) x (integer? x) (new clojure.lang.Ratio (bigint x) (bigint 1)))))

(defn numeric-singleton-type
  "Creates a singleton type that compares by regular equality."
  [x]
  (new DataType
    1
    #(if (= % 1) x)
    #(if (= % x) 1)
    #(= x %)))

(def POSITIVE-RATIONALS
  (let [raw-type (cartesian-product-type BOOLEANS (without NATURAL-LISTS [])),
        ONE (ratio 1),
        move-right (fn [q steps]
                     (let [a (numerator q), b (denominator q)]
                       (ratio (+ a (* steps b)) b))),
        move-left (fn [q steps]
                    (let [a (numerator q), b (denominator q)]
                      (ratio a (+ b (* steps a)))))]
    (union-type
      (wrap-type raw-type
        (fn [[left-first step-seq]]
          (first
            (reduce
              (fn [[q left] steps]
                [((if left move-left move-right) q steps) (not left)])
              [ONE left-first]
              step-seq)))
        (fn [q]
          (let [q (ratio q)]
            (loop [a (numerator q), b (denominator q), step-seq ()]
              (cond
                (= 1 a)
                  [true (cons (dec b) step-seq)]
                (= 1 b)
                  [false (cons (dec a) step-seq)]
                (> a b)
                  (recur (mod a b) b (cons (quot a b) step-seq))
                (< a b)
                  (recur a (mod b a) (cons (quot b a) step-seq))))))
        (fn [q] (and (or (ratio? q) (integer? q)) (> q 0) (not= 1 q))))
      (numeric-singleton-type ONE))))

(def RATIONALS
  (let [neg-rats (wrap-type POSITIVE-RATIONALS - - (comp (partial element? POSITIVE-RATIONALS) -))]
    (union-type POSITIVE-RATIONALS neg-rats (numeric-singleton-type (ratio 0)))))

(def HEX-STRINGS
  (strings-with-chars "0123456789abcdef"))

(defn natural-permutations
  "Returns a finite type of all the permutations of the numbers from 0 to n-1"
  [n]
  (let [card (apply * (range 1 (inc n)))]
    (new DataType
      card
      (fn [nn]
        (loop [modulus n, i (dec nn), res (), nums (apply sorted-set (range n))]
          (if (empty? nums)
            res
            (let [[a b] ((juxt quot rem) i modulus),
                  x (nth (seq nums) b)]
              (recur
                (dec modulus)
                a
                (conj res x)
                (disj nums x))))))
      (fn [p]
        (inc
          (first
            (reduce
              (fn [[i nums] pi]
                (let [choice (count (take-while #(< % pi) nums))]
                  [(+ choice (* i (inc (count nums)))) (conj nums pi)]))
              [0 (sorted-set)]
              p))))
      (fn [p]
        (and (coll? p) (= n (count p)) (= (set p) (set (range n))))))))

(defn non-repeating-natural-lists
  "Returns a type of lists of natural numbers from 1 to n such that
  no consecutive numbers are the same."
  [n]
  (let [raw-type (cartesian-product-type
                   (finite-nats n)
                   (lists-of (finite-nats (dec n))))]
    (with
      (wrap-type raw-type
        (fn [[init others]]
          (reduce
            (fn [so-far next-num]
              (conj so-far
                (if (>= next-num (last so-far)) (inc next-num) next-num)))
            [init]
            others))
        (fn [[init & others]]
          (list
            init
            (last
              (reduce
                (fn [[last-value new-list] el]
                  (let [v (if (> el last-value) (dec el) el)]
                    (vector v (conj new-list v))))
                    
                [init []]
                others))))
        (fn [coll]
          (and
            (every? #(and (natural? %) (<= % n)) coll)
            (every? #(not= (first %) (second %)) (partition 2 1 coll)))))
      [])))


;;
;; Money-CSV
;;

(defn- natural-range-type
  [min max]
  {:pre [(< min max)]}
  (wrap-type (finite-nats (inc (- max min)))
    (fn [finite-nat] (dec (+ finite-nat min)))
    (fn [n] (inc (- n min)))
    (fn [n] (and (integer? n) (<= min n max)))))

(defn- csv-row-type
  [cols min-cents max-cents]
  (let [t (natural-range-type min-cents max-cents)]
    (apply cartesian-product-type (repeat cols t))))

(defn min-length-lists-of
  [t length]
  (let [lot (lists-of t)]
    (wrap-type (cartesian-product-type (tuples-of length t) lot)
      (partial apply concat)
      (partial split-at length)
      (fn [coll] (and (element? lot coll) (>= (count coll) length))))))

(defn- fixed-width-money-csv
  [cols min-rows min-cents max-cents]
  (let [row-type (csv-row-type cols min-cents max-cents)]
    (min-length-lists-of row-type min-rows)))

(defn money-csv-raw
  "Returns a type that enumerates all CSV files of filled rectangular
   shape of money-looking values, i.e. numbers with two decimal places."
  [min-cols max-cols min-rows min-cents max-cents]
  (let [fixed-width-type (memoize #(fixed-width-money-csv % min-rows min-cents max-cents)),
        col-count-type (natural-range-type min-cols max-cols)]
    (wrap-type (cartesian-product-type col-count-type NATURALS)
      (fn [[col-count n]] (to (fixed-width-type col-count) n))
      (fn [rows]
        (let [col-count (-> rows first count)]
          [col-count (from (fixed-width-type col-count) rows)]))
      (fn [rows]
        (let [col-count (-> rows first count)]
          (element? (fixed-width-type col-count) rows))))))

(defn money-csv
  [& args]
  (let [int-to-decimal-string #(format "%.2f" (double (/ % 100))),
        decimal-string-to-int #(int (Math/round (* 100 (new Double %))))]
    (wrap-type (apply money-csv-raw args)
      (fn [rows]
        (string/join "\n"
          (for [row rows]
            (string/join "," (map int-to-decimal-string row)))))
      (fn [s]
        (let [row-strings (string/split s #"\n")]
          (for [row-string row-strings]
            (map decimal-string-to-int (string/split row-string #",")))))
      (fn [s] true))))
