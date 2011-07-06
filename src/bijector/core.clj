(ns bijector.core)

(defprotocol IDataType
  (cardinality [this])
  (to [this n])
  (from [this x]))

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
