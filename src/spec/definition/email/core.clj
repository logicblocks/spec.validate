(ns spec.definition.email.core
  (:require
   [datatype.email.core :as dt-email]
   [datatype.email.gen :as dt-email-gen]

   [spec.definition.core :as sd]))

(sd/def-spec :datatype.email/email-address
  {:pred dt-email/email-address?
   :gen dt-email-gen/gen-email-address
   :req :must-be-an-email-address})
