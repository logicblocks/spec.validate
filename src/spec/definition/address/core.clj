(ns spec.definition.address.core
  (:require
   [datatype.address.core :as dt-address]
   [datatype.address.gen :as dt-address-gen]

   [spec.definition.core :as sd]))

(declare
  uk-postcode-formatted-string?)

(sd/def-validate-pred uk-postcode-formatted-string?
  "Returns true if the provided value is a string representing a UK postcode,
  else returns false."
  [value]
  {:requirement :must-be-a-uk-postcode
   :gen         dt-address-gen/gen-uk-postcode-formatted-string}
  (dt-address/uk-postcode-formatted-string? value))
