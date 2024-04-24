(ns spec.definition.currency.core
  (:require
   [clojure.spec.gen.alpha :as gen]

   [datatype.currency.core :as dt-currency]

   [icu4clj.util.currency :as icu-c]

   [spec.definition.core :as sd]))

(declare
  iso4217-currency-code-string?

  ; monetary-amount-string?
  )

(defn gen-currency-code []
  (gen/elements (icu-c/available-currency-codes)))

(sd/def-validate-pred iso4217-currency-code-string?
  "Returns true if the provided value is an ISO-4217 currency code string,
  else returns false."
  [value]
  {:requirement :must-be-an-iso4217-currency-code-string
   :gen         gen-currency-code}
  (dt-currency/iso4217-currency-code-string? value))
