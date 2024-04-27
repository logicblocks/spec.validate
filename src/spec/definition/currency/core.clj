(ns spec.definition.currency.core
  (:require
   [datatype.currency.core :as dt-currency]
   [datatype.currency.gen :as dt-currency-gen]

   [spec.definition.core :as sd]))

(declare
  iso4217-currency-code-string?

  ; monetary-amount-string?
  )

(sd/def-validate-pred iso4217-currency-code-string?
  "Returns true if the provided value is an ISO-4217 currency code string,
  else returns false."
  [value]
  {:requirement :must-be-an-iso4217-currency-code-string
   :gen         dt-currency-gen/gen-currency-code}
  (dt-currency/iso4217-currency-code-string? value))
