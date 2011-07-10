(ns bijector.string-partitions
  "Helper functions for fixed-length partitions.")

(def string-partitions
  (memoize
    (fn [length partitions]
      {:pre [(not (neg? length)) (pos? partitions)]}
      (cond
        (or (= 0 length) (= 1 partitions))
          1
        (= 1 length)
          partitions
        :else
          (apply + (for [x (range (inc length))]
                     (string-partitions
                       (- length x)
                       (dec partitions))))))))

(defn nth-partition
  [s partitions n]
  {:pre [(<= n (string-partitions (count s) partitions))],
   :post [(= s (apply str %))]}
  (cond
    (= partitions 1)
      [s]
    (empty? s)
      [""]
    :else
      (loop [n n, c [], s s, partitions partitions]
        (if (= 1 partitions)
          (do
            (assert (= 1 n))
            (conj c s))
          (let [[part-length n]
                  (loop [part-length 0, n n]
                    (let [k (string-partitions
                              (- (count s) part-length)
                              (dec partitions))]
                      (if (> n k)
                        (recur (inc part-length) (- n k))
                        [part-length n])))]
            (recur
              n
              (conj c (.substring s 0 part-length))
              (.substring s part-length)
              (dec partitions)))))))

(defn partition-lengths-to-n
  [a]
  (if (< (count a) 2)
    1
    (let [total (apply + a)]
      (apply +
        (partition-lengths-to-n (butlast a))
        (for [x (range (last a))]
          (string-partitions (- total x) (dec (count a))))))))

(defn binary-partition-to-n
  [ss])

(defn- zero-padded
  [n length]
  (let [s (.toString (bigint n) 2),
        s (str
            (apply str (repeat (- length (count s)) "0"))
            s)]
    (.substring s 0 length)))

; this ain't working right
(defn n-to-binary-partition
  [partitions n]
  (let [[str-length n]
         (loop [str-length 0, n n, p 1]
           (let [k (* (string-partitions str-length partitions) p)]
             (if (> n k)
               (recur (inc str-length) (- n k) (* p 2))
               [str-length n]))),
        k (string-partitions str-length partitions),
        s (zero-padded (dec n) str-length)]
    (for [part (nth-partition s partitions (inc (rem (dec n) k)))]
      (new BigInteger (str "1" part) 2))))
