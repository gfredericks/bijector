(ns bijector.examples
  (:import (bijector.core DataType InfiniteDataType EnumerationDataType))
  (:use bijector.core))

(defn finite-nats
  [max-value]
  (new DataType
    (constantly max-value)
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
