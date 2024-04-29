(ns spec.definition.address.core
  (:require
   [datatype.address.core :as dt-address]
   [datatype.address.gen :as dt-address-gen]

   [spec.definition.core :as sd]))

(sd/def-spec :datatype.address/uk-postcode-formatted-string
  {:pred dt-address/uk-postcode-formatted-string?
   :gen dt-address-gen/gen-uk-postcode-formatted-string
   :req :must-be-a-uk-postcode})
