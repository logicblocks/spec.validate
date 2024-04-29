(ns spec.definition.currency.core
  (:require
   [datatype.currency.core :as dt-currency]
   [datatype.currency.gen :as dt-currency-gen]

   [spec.definition.core :as sd]))

(sd/def-spec :datatype.currency/iso4217-currency-code-string
  {:pred dt-currency/iso4217-currency-code-string?
   :gen dt-currency-gen/gen-currency-code
   :req :must-be-an-iso4217-currency-code-string})
