(ns spec.definition.phone.core
  (:require
   [datatype.phone.core :as dt-phone]
   [datatype.phone.gen :as dt-phone-gen]

   [spec.definition.core :as sd]))

(sd/def-spec :datatype.phone/phone-number-string
  {:pred dt-phone/phone-number-string?
   :gen dt-phone-gen/gen-phone-number
   :req :must-be-a-phone-number-string})
