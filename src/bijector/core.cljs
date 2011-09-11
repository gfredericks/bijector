(ns bijector.core
  (:require [goog.math.Integer :as mathint]))

(defn bigint
  [n]
  (mathint/fromString (str n)))
