(ns datatype.currency.core
  (:require
   [datatype.support :as dts]

   [icu4clj.util.currency :as icu-c]))

(defn iso4217-currency-code-string? [value]
  (dts/exception->false
    (contains?
      (set (icu-c/available-currency-codes))
      value)))
