(ns spec.validate.money
  (:require
   [clojure.spec.gen.alpha :as gen]
   [icu4clj.util.currency :as icu-c]

   [spec.validate.utils :as sv-utils]))

(declare
  iso4217-currency-code-string?

  ; monetary-amount-string?
  )

(sv-utils/def-validate-pred iso4217-currency-code-string?
  "Returns true if the provided value is an ISO-4217 currency code string,
  else returns false."
  [value]
  {:requirement :must-be-an-iso4217-currency-code-string
   :gen         #(gen/elements (icu-c/available-currency-codes))}
  (contains?
    (set (icu-c/available-currency-codes))
    value))
