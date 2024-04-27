(ns spec.definition.phone.core
  (:require
   [datatype.phone.core :as dt-phone]
   [datatype.phone.gen :as dt-phone-gen]

   [spec.definition.core :as sd]))

(declare
  phone-number-string?)

(sd/def-validate-pred phone-number-string?
  "Returns true if the provided value is a string representing a phone number,
  else returns false. By default, treats phone numbers as being from
  Great Britain, however the default region can be overridden with
  `*default-region-code*`."
  [value]
  {:requirement :must-be-a-phone-number-string
   :gen         dt-phone-gen/gen-phone-number}
  (dt-phone/phone-number-string? value))
