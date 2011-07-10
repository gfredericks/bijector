(ns bijector.core
  (:require [bijector.string-partitions :as parts]))

(defprotocol IDataType
  (cardinality [this])
  (to [this n])
  (from [this x])
  (element? [this x]))

(defn infinite? [t] (= :infinity (cardinality t)))
(def finite? (complement infinite?))

(defn make-converters-between
  [t1 t2]
  [(comp (partial to t2) (partial from t1))
   (comp (partial to t1) (partial from t2))])

(defrecord DataType [s t f e]
  IDataType
  (cardinality [_] (s))
  (to [_ n] (t n))
  (from [_ x] (f x))
  (element? [_ x] (e x)))

(defrecord InfiniteDataType [t f e]
  IDataType
  (cardinality [_] :infinity)
  (to [_ n] (t n))
  (from [_ x] (f x))
  (element? [_ x] (e x)))

(defrecord EnumerationDataType [elements]
  IDataType
  (cardinality [_] (count elements))
  (to [_ n]
    (try
      (nth elements (dec n))
      (catch IndexOutOfBoundsException e
        (throw (new Exception (format "Bad argument (to %s %s)" (pr-str elements) (pr-str n)))))))
  ; TODO -- this definitely ought to be memoized, right?
  (from [_ x]
    (let [[_ n] (first (filter #(= x (first %)) (map vector elements (rest (range)))))]
      (or n (throw (new Exception (format "%s is not an element of %s" (pr-str x) (pr-str elements)))))))
  ; TODO -- this definitely ought to be memoized, right?
  (element? [_ x]
    (contains? (set elements) x)))

(def BOOLEANS (new EnumerationDataType [true false]))

(def NATURALS
  (new InfiniteDataType
    identity
    identity
    #(and (integer? %) (pos? %))))

(def INTEGERS
  (new InfiniteDataType
    (fn [n]
      (cond (= 1 n) 0, (even? n) (/ n 2), (odd? n) (- (/ (dec n) 2))))
    (fn [x]
      (cond (zero? x) 1, (neg? x) (inc (* 2 (- x))), (pos? x) (* 2 x)))
    integer?))

(declare NATURAL-LISTS)

(defn lists-of
  "Creates a new type which is arbitrary-length lists of elements of the
  given type."
  [t]
  (if (finite? t)
    ; TODO: Performance tuning -- this should be faster with subtraction for
    ;       small cardinalities, but not sure exactly where the boundary is;
    ;       could figure that out with tests
    (let [c (cardinality t)]
      (new InfiniteDataType
        (fn [n]
          (loop [n n, [p & ps :as ps*] '(1)]
            (if (> n p)
              (recur
                (- n p)
                (cons (* p c) ps*))
              (loop [n (dec n), [p & ps] ps, res ()]
                (if p
                  ; TODO: We could do this faster if we knew the args were bigints
                  ;       (BigInteger#divideAndRemainder)
                  (let [[a b] ((juxt quot rem) n p)]
                    (recur b ps (conj res (to t (inc a)))))
                  res)))))
        (fn [xs]
          (let [smaller-lists (apply + (take (count xs) (iterate #(* c %) 1)))]
            (loop [b 1, n (inc smaller-lists), [x & xs :as xs*] xs]
              (if (empty? xs*)
                n
                (recur
                  (* b c)
                  (+ n (* b (dec (from t x))))
                  xs)))))
        (fn [coll]
          (and
            (sequential? coll)
            (every? #(element? t %) coll)))))
    (new InfiniteDataType
      (fn [n] (map (partial to t) (to NATURAL-LISTS n)))
      (fn [xs] (from NATURAL-LISTS (map (partial from t) xs)))
      (fn [coll]
        (and
          (sequential? coll)
          (every? #(element? t %) coll))))))

(def NATURAL-LISTS
  (let [TERNARY (lists-of (new EnumerationDataType [0 1 2])),
        BINS    (lists-of (new EnumerationDataType [0 1])),
        split-on-twos
          (fn [coll]
            (loop [xs coll, ret []]
              (if (empty? xs)
                (conj ret [])
                (let [[a b] (split-with (complement #{2}) xs),
                      cra (conj ret a)]
                  (if (empty? b)
                    cra
                    (recur (rest b) cra))))))]
    (new InfiniteDataType
      (fn [n]
        (if (= 1 n)
          []
          (->>
            n
            (dec)
            (to TERNARY)
            (split-on-twos)
            (map (partial from BINS)))))
      (fn [xs]
        (if (empty? xs)
          1
          (->>
            xs
            (map (partial to BINS))
            (interpose 2)
            (flatten)
            (from TERNARY)
            (inc))))
      (fn [coll]
        (and
          (sequential? coll)
          (every? #(element? NATURALS %) coll))))))

#_(defn tuples-of
  "Like lists-of, but for lists of a fixed size."
  [t size]
  (if (finite? t)
    (new DataType)))


(defn finite-union-type
  "Arguments should be constant functions returning the type.
  This delayed evaluation allows recursive types."
  [ft1 ft2]
  (let [c1 (delay (cardinality (ft1))),
        c2 (delay (cardinality (ft2))),
        c3 (delay (if (= :infinity c2) c2 (+ c1 c2))),
        t1 (delay (ft1)),
        t2 (delay (ft2))]
    (new DataType
      (partial deref c3)
      (fn [n] (if (> n @c1) (to @t2 (- n @c1)) (to @t1 n)))
      (fn [x]
        (if (element? @t1 x)
          (from @t1 x)
          (+ @c1 (from @t2 x))))
      (fn [x] (or (element? @t1 x) (element? @t2 x))))))

(defn infinite-union-type
  "Arguments should be constant functions returning the type.
  This delayed evaluation allows recursive types."
  [ft1 ft2]
  (let [t1 (delay (ft1)),
        t2 (delay (ft2))]
    (new InfiniteDataType
      (fn [n]
        (if (odd? n)
          (to @t1 (/ (inc n) 2))
          (to @t2 (/ n 2))))
      (fn [x]
        (if (element? @t1 x)
          (dec (* 2 (from @t1 x)))
          (* 2 (from @t2 x))))
      (fn [x] (or (element? @t1 x) (element? @t2 x))))))

(defn union-type
  [t1 t2]
  (cond
    (finite? t1)
      (finite-union-type (constantly t1) (constantly t2))
    (finite? t2)
      (union-type t2 t1)
    :else
      (infinite-union-type (constantly t1) (constantly t2))))

(defn binary-partitions-type
  [partitions]
  (new InfiniteDataType
    (partial parts/n-to-binary-partition partitions)
    (partial parts/binary-partition-to-n partitions)
    (fn [coll]
      (and
        (= partitions (count coll))
        (every? #(re-matches #"[01]*" %) coll)))))

#_(defn cartesian-product-type
  [& ts]
  (let [card (if (some infinite? ts) :infinity (reduce * (map cardinality ts)))]
    (new DataType
      (constantly card)
      (fn [n]))))

(defn strings-with-chars
  [chars]
  (let [char-lists (lists-of (new EnumerationDataType chars)),
        char-set (set chars)]
    (new InfiniteDataType
      (fn [n] (apply str (to char-lists n)))
      (partial from char-lists)
      (fn [s] (and (string? s) (every? #(contains? char-set %) s))))))

(def SIMPLE-ASCII
  (strings-with-chars
    " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\n\r"))

; Idea: this doesn't seem to be a very even definition,
;       for example note the superexponential growth of
;       the sequence [1], [[1]], [[[1]]], [[[[1]]]], ...
;       when it ought to only be exponential. There might
;       be cool tricks we could do similar to how the
;       NATURAL-LISTS type was defined, but using base 5 or
;       something like that.
(def NESTED-NATURAL-LISTS
  (lists-of
    (infinite-union-type
      (constantly NATURALS)
      (fn [] NESTED-NATURAL-LISTS))))
