(ns bijector.core
  (:use [clojure.contrib.seq-utils :only [separate]])
  (:require [bijector.string-partitions :as parts]))

(defprotocol IDataType
  (cardinality [this])
  (to [this n])
  (from [this x])
  (element? [this x]))

(defn infinite? [t] (= :infinity (cardinality t)))
(def finite? (complement infinite?))
(def natural? #(and (integer? %) (pos? %)))

(defn make-converters-between
  [t1 t2]
  [(comp (partial to t2) (partial from t1))
   (comp (partial to t1) (partial from t2))])

(defmacro def-converters
  "E.g.,
    (defconverters to-int to-ascii INTEGERS SIMPLE-ASCII)"
  [name1 name2 type1 type2]
  `(let [[f1# f2#] (make-converters-between ~type1 ~type2)]
     (def ~name1 f2#)
     (def ~name2 f1#)))

(defrecord DataType [card t f e]
  IDataType
  (cardinality [_] card)
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

(defn wrap-type
  "Helper function for wrapping a type in transformation functions.
  Creates a type of the same size as t, where the function from-t
  transforms an instance of type t to an instance of the wrapped type,
  and to-t transforms and instance of the wrapped type to type t."
  [t from-t to-t recognizer]
  (new DataType
    (cardinality t)
    (comp from-t (partial to t))
    (comp (partial from t) to-t)
    recognizer))

(def BOOLEANS (new EnumerationDataType [true false]))

(def NATURALS
  (new InfiniteDataType
    identity
    identity
    natural?))

(def INTEGERS
  (new InfiniteDataType
    (fn [n]
      (cond (= 1 n) 0, (even? n) (/ n 2), (odd? n) (- (/ (dec n) 2))))
    (fn [x]
      (cond (zero? x) 1, (neg? x) (inc (* 2 (- x))), (pos? x) (* 2 x)))
    integer?))

(defn integer-range-type
  "Takes one or two arguments, with same meaning as clojure.core/range"
  ([end] (integer-range-type 0 end)) 
  ([start end]
    {:pre [(< start end)]}
    (new DataType
      (- end start)
      (fn [n] (+ start (dec n)))
      (fn [n] (inc (- n start)))
      #(and (natural? %) (>= % start) (< % end)))))

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

(def NATURAL-SETS
  (wrap-type
    NATURAL-LISTS
    (fn [n-list]
      (set
        (map + n-list (reductions + 0 n-list))))
    (fn [n-set]
      (let [in-order (sort n-set)]
        (map - in-order (cons 0 in-order))))
    (fn [coll] (and (set? coll) (every? natural? coll)))))

(defn- sets-of-infinite-type
  [t]
  (wrap-type
    NATURAL-SETS
    (fn [set-of-n] (set (map #(to t %) set-of-n)))
    (fn [set-of-t] (set (map #(from t %) set-of-t)))
    (fn [coll] (and (set? coll) (every? #(element? t %) coll)))))

(defn- sets-of-finite-type
  [t]
  (let [tcard (cardinality t),
        card (apply * (repeat tcard 2))]
    (new DataType
      card
      (fn [n]
        (set
          (let [n (bigint (dec n))]
            (for [x (range tcard), :when (.testBit n x)]
              (to t (inc x))))))
      (fn [obs]
        (inc
          (reduce
            (fn [n ob]
              (.setBit n (dec (from t ob))))
            (bigint 0)
            obs)))
      (fn [obs] (and (set? obs) (every? #(element? t %) obs))))))

(defn sets-of
  [t]
  ((if (finite? t) sets-of-finite-type sets-of-infinite-type) t))

(defn binary-partitions-type
  [partitions]
  (new InfiniteDataType
    (partial parts/n-to-binary-partition partitions)
    (partial parts/binary-partition-to-n partitions)
    (fn [coll]
      (and
        (= partitions (count coll))
        (every? #(re-matches #"[01]*" %) coll)))))

(defn natural-tuples-type
  [length]
  (wrap-type
    (binary-partitions-type length)
    (fn [ss]
      (for [s ss] (new BigInteger (str "1" s) 2)))
    (fn [ns]
      (for [n ns] (.substring (.toString (bigint n) 2) 1)))
    (fn [coll]
      (and (= length (count coll)) (every? natural? coll)))))

(defn sequence-has-types?
  "Checks that the given sequence is the same length as the given type-sequence,
  and that each member of the sequence is an element of its corresponding type."
  [ts coll]
  (and
    (= (count ts) (count coll))
    (every? (fn [[x t]] (element? t x)) (map vector coll ts))))

(defn finite-cartesian-product-type
  [& ts]
  {:pre [(not (empty? ts))
         (every? finite? ts)]}
  (if (= 1 (count ts))
    (wrap-type
      (first ts)
      list
      first
      #(and (= 1 (count %)) (element? (first ts) (first %))))
    (let [c (apply * (map cardinality ts))]
      (new DataType
        c
        (fn [n]
          (first
            (reduce
              (fn [[so-far n] t]
                (let [c (cardinality t)]
                  [(conj so-far (to t (inc (rem n c))))
                   (quot n c)]))
              [[] (dec n)]
              ts)))
        (fn [coll]
          (inc
            (second
              (reduce
                (fn [[multiple n] [v t]]
                  (let [c (cardinality t)]
                    [(* multiple c)
                     (+ n (* multiple (dec (from t v))))]))
                [1 0]
                (map vector coll ts)))))
        (partial sequence-has-types? ts)))))

(defn infinite-cartesian-product-type
  [& ts]
  {:pre [(not (empty? ts))
         (every? infinite? ts)]}
  (if (= 1 (count ts))
    (wrap-type
      (first ts)
      list
      first
      #(and (= 1 (count %)) (element? (first ts) (first %))))
    (wrap-type
      (natural-tuples-type (count ts))
      (fn [ns]
        (for [[n t] (map vector ns ts)] (to t n)))
      (fn [coll]
        (for [[x t] (map vector coll ts)] (from t x)))
      (partial sequence-has-types? ts))))

(defn cartesian-product-type
  [& ts]
  {:pre [(not (empty? ts))]}
  (let [[finites infinites] (separate (comp finite? first) (map vector ts (range))),
        finite-product
          (if-not (empty? finites)
            (apply finite-cartesian-product-type (map first finites))),
        infinite-product
          (if-not (empty? infinites)
            (apply infinite-cartesian-product-type (map first infinites)))]
    (cond
      (empty? finites) infinite-product
      (empty? infinites) finite-product
      :else
        (let [finite-card (cardinality finite-product)]
          (new InfiniteDataType
            (fn [n]
              (let [n (dec n),
                    finite-n (rem n finite-card),
                    infinite-n (quot n finite-card),
                    finite-val (to finite-product (inc finite-n)),
                    infinite-val (to infinite-product (inc infinite-n)),
                    finite-with-index (map vector finite-val (map last finites)),
                    infinite-with-index (map vector infinite-val (map last infinites))]
                (->>
                  (concat finite-with-index infinite-with-index)
                  (sort-by last)
                  (map first))))
            (fn [coll]
              (let [x-with-type (map vector coll ts),
                    [finites infinites] (separate (comp finite? second) x-with-type),
                    finite-n (from finite-product (map first finites)),
                    infinite-n (from infinite-product (map first infinites))]
                (inc (+ (dec finite-n) (* (dec infinite-n) finite-card)))))
            (partial sequence-has-types? ts))))))

(defn finite-union-type
  [t1 t2]
  (let [c1 (cardinality t1),
        c2 (cardinality t2),
        c3 (if (= :infinity c2) c2 (+ c1 c2))]
    (new DataType
      c3
      (fn [n] (if (> n c1) (to t2 (- n c1)) (to t1 n)))
      (fn [x]
        (if (element? t1 x)
          (from t1 x)
          (+ c1 (from t2 x))))
      (fn [x] (or (element? t1 x) (element? t2 x))))))

(defn infinite-union-type
  [& ts]
  (let [ts (vec ts)]
    (wrap-type
      (cartesian-product-type
        (integer-range-type (count ts))
        NATURALS)
      (fn [[t-index n]] (to (nth ts t-index) n))
      (fn [x]
        (loop [t-index 0]
          (if (element? (nth ts t-index) x)
            [t-index (from (nth ts t-index) x)]
            (recur (inc t-index)))))
      (fn [x] (boolean (some #(element? % x) ts))))))

(defn union-type
  [& ts]
  {:pre [(< 1 (count ts))]}
  (let [[finites infinites] (separate finite? ts),
        combine-finite-types
          (fn combine-finite-types [ts]
            (cond
              (= 1 (count ts))
                (first ts)
              (= 2 (count ts))
                (apply finite-union-type ts)
              (< 2 (count ts))
                (let [[ts1 ts2] (split-at (quot (count ts) 2) ts)]
                  (finite-union-type
                    (combine-finite-types ts1)
                    (combine-finite-types ts2)))))]
    (cond
      (empty? finites)
        (apply infinite-union-type infinites)
      (empty? infinites)
        (combine-finite-types finites)
      :else
        (finite-union-type
          (combine-finite-types finites)
          (apply infinite-union-type infinites)))))

(defn tuples-of
  [size t]
  (apply cartesian-product-type (repeat size t)))

(def pairs-of (partial tuples-of 2))

(defn without
  "Returns a new type without the specified elements."
  [t & elements]
  (let [indices (sort (map #(from t %) elements)),
        el-set (set elements)]
    (new DataType
      (if (finite? t) (- (cardinality t) (count elements)) :infinity)
      (fn [n]
        (to t
          (loop [indices indices, n n]
            (if (or (empty? indices) (< n (first indices)))
              n
              (recur (rest indices) (inc n))))))
      (fn [x]
        (let [n (from t x)]
          (- n (count (take-while #(< % n) indices)))))
      (fn [x] (and (element? t x) (not (el-set x)))))))

(defn with
  "Returns a new type with the listed elements added."
  [t & elements]
  (union-type t (new EnumerationDataType elements)))

(defn strings-with-chars
  [chars]
  (let [char-lists (lists-of (new EnumerationDataType chars)),
        char-set (set chars)]
    (new InfiniteDataType
      (fn [n] (apply str (to char-lists n)))
      (partial from char-lists)
      (fn [s] (and (string? s) (every? #(contains? char-set %) s))))))

(def simple-ascii-chars
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\n\r")

(def SIMPLE-ASCII (strings-with-chars simple-ascii-chars))
    
(defn stub-type
  "Takes a function that returns an infinite type, and returns a stub type that
  will call that function once the first time it is needed. This allows
  recursive types."
  [f]
  (let [t (delay (f))]
    (new InfiniteDataType
      (fn [n] (to @t n))
      (fn [x] (from @t x))
      (fn [x] (element? @t x)))))

(let
  [catalan
     (memoize
       (fn [n] (apply * (for [k (range 2 (inc n))] (/ (+ n k) k))))),
   trees-with-order (comp catalan dec),
   ! (memoize (fn [n] (apply * (range 1 (inc n))))),
   catalan-triangle
     (fn [n k]
       (let [n (dec n), k (- n k -1)]
         (/
           (*
             (! (+ n k))
             (inc (- n k)))
           (! k)
           (! (inc n)))))]
  (letfn
    [(nth-tree-with-order
       [order n]
       (if (= 1 order)
         (if (> 1 n)
           (throw (new Exception "CRAZY"))
           [])
         (loop [degree 1, n n]
           (let [c (trees-with-degree order degree)]
             (if (> n c)
               (recur (inc degree) (- n c))
               (nth-tree-with-degree order degree n))))))
     (tree-order
       [coll]
       (inc (apply + (map tree-order coll))))
     (tree-to-nth-of-order
       [order tree]
       {:pre [(pos? order)]}
       (if (and (= 1 order) (empty? tree))
         1
         (let [degree (count tree)]
           (+ (trees-with-smaller-degree order degree)
              (tree-to-nth-of-degree order degree tree)))))
     (trees-with-degree
       [order degree]
       (let [n (dec order)
             k degree]
         (catalan-triangle n k)))
     (nth-tree-with-degree
       [order degree n]
       (if (= 1 degree)
         [(nth-tree-with-order (dec order) n)]
         (let [free-vertices (- order 1 degree)]
           ; first order is between 1 and (inc free-vertices) inclusive
           ; first-order is the order of the first child, including the first
           ; child's root
           (loop [first-order (inc free-vertices), n n]
             (let [too (trees-with-order first-order),
                   twd (trees-with-degree (- order first-order) (dec degree)),
                   number-of-these (* too twd)]
               (if (> n number-of-these)
                 (recur (dec first-order) (- n number-of-these))
                 (let [pair-type (cartesian-product-type
                                   (integer-range-type 1 (inc too))
                                   (integer-range-type 1 (inc twd))),
                       [first-index rest-index] (to pair-type n),
                       first-tree (nth-tree-with-order first-order first-index),
                       rest-forest (nth-tree-with-degree (- order first-order) (dec degree) rest-index)]
                   (vec (cons first-tree rest-forest))))))))),
     (tree-to-nth-of-degree
       [order degree tree]
       {:pre [(pos? order) (pos? degree)]}
       (if (= 1 degree)
         (tree-to-nth-of-order (dec order) (first tree))
         (let [first-order (tree-order (first tree)),
               rest-order (- order first-order),
               first-cardinality (trees-with-order first-order),
               rest-cardinality (trees-with-degree rest-order (dec degree)),
               trees-with-larger-first-order
                 (apply +
                   (for [forder (range (inc first-order) (inc (- order degree)))]
                     (* (trees-with-order forder)
                        (trees-with-degree (- order forder) (dec degree))))),
               pair-type (cartesian-product-type
                           (integer-range-type 1 (inc first-cardinality))
                           (integer-range-type 1 (inc rest-cardinality)))]
           (+ trees-with-larger-first-order
              (from pair-type [(tree-to-nth-of-order first-order (first tree))
                               (tree-to-nth-of-degree rest-order (dec degree) (rest tree))])))))
     (trees-with-smaller-order
       [order]
       (apply + (for [o (range 1 order)] (trees-with-order o))))
     (trees-with-smaller-degree
       [order degree]
       (apply + (for [d (range 1 degree)] (trees-with-degree order d))))]
    (def EMPTY-LISTS
      (new InfiniteDataType
        (fn [n]
          (loop [order 1, n n]
            (let [too (trees-with-order order)]
              (if (> n too)
                (recur (inc order) (- n too))
                (nth-tree-with-order order n)))))
        (fn f [coll]
          (if (empty? coll)
            1
            (let [order (tree-order coll)]
              (+ (trees-with-smaller-order order)
                 (tree-to-nth-of-order order coll)))))
        (fn f [coll]
          (and (sequential? coll) (every? f coll)))))))

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
      NATURALS
      (stub-type (fn [] NESTED-NATURAL-LISTS)))))
