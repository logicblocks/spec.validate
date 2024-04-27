(ns datatype.currency.core
  (:require
   [datatype.support :as dts]

   [icu4clj.util.currency :as icu-c]))

(defn iso4217-currency-code-string?
  "Returns true if the provided value is an ISO-4217 currency code string,
  else returns false."
  [value]
  (dts/exception->false
    (contains?
      (set (icu-c/available-currency-codes))
      value)))
