(ns bijector.examples
  (:import (bijector.core DataType InfiniteDataType EnumerationDataType))
  (:use bijector.core))

(defn finite-nats
  [max-value]
  (new DataType
    max-value
    identity
    identity
    #(and (natural? %) (<= % max-value))))

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
