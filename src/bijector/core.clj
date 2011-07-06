(ns bijector.core)

(defprotocol IDataType
  (cardinality [this])
  (to [this n])
  (from [this x]))

(defn infinite? [t] (= :infinity (cardinality t)))
(def finite? (complement infinite?))

(defrecord DataType [s t f]
  IDataType
  (cardinality [_] (s))
  (to [_ n] (t n))
  (from [_ x] (f x)))

(defrecord InfiniteDataType [t f]
  IDataType
  (cardinality [_] :infinity)
  (to [_ n] (t n))
  (from [_ x] (f x)))

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
      (or n (throw (new Exception (format "%s is not an element of %s" (pr-str x) (pr-str elements))))))))

(def BOOLEANS (new EnumerationDataType [true false]))

(def NATURALS
  (new InfiniteDataType
    identity
    identity))

(def INTEGERS
  (new InfiniteDataType
    (fn [n]
      (cond (= 1 n) 0, (even? n) (/ n 2), (odd? n) (- (/ (dec n) 2))))
    (fn [x]
      (cond (zero? x) 1, (neg? x) (inc (* 2 (- x))), (pos? x) (* 2 x)))))

(declare NATURAL-LISTS)

(defn list-of
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
          (loop [n n, [p & ps :as ps*] '(1), l 0]
            (if (> n p)
              (recur
                (- n p)
                (cons (* p c) ps*)
                (inc l))
              (loop [n n, [p & ps] ps, res []]
                (if p
                  ; TODO: We could do this faster if we knew the args were bigints
                  ;       (BigInteger#divideAndRemainder)
                  (let [[a b] ((juxt quot rem) n p)]
                    (recur b ps (conj res (to t (inc a)))))
                  res)))))
        (fn [xs]
          (let [smaller-lists (apply + (take (count xs) (iterate #(* c %) 1)))]
            (loop [b 1, n smaller-lists, [x & xs :as xs*] xs]
              (if (empty? xs*)
                n
                (recur
                  (* b c)
                  (+ n (* b (dec (from t x))))
                  xs)))))))
    (new InfiniteDataType
      (fn [n] (map (partial to t) (to NATURAL-LISTS n)))
      (fn [xs] (from NATURAL-LISTS (map (partial from t) xs))))))

(def NATURAL-LISTS
  (let [TERNARY (new EnumerationDataType [0 1 2]),
        BINS    (new EnumerationDataType [0 1]),
        split-on-twos
          (fn [coll]
            (loop [xs coll, ret []]
              (if (empty? xs)
                (conj ret [])
                (let [[a b] (split-with (complement #{2}) xs)]
                  (recur (rest b) (conj ret a))))))]
    (new InfiniteDataType
      (fn [n]
        (->> n (to TERNARY) split-on-twos (map (partial from BINS))))
      (fn [xs]
        (->> xs (map (partial to BINS)) (interpose 2) (from TERNARY))))))

