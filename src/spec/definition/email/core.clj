(ns spec.definition.email.core
  (:require
   [datatype.email.core :as dt-email]
   [datatype.email.gen :as dt-email-gen]

   [spec.definition.core :as sd]))

(declare
  email-address?)

(sd/def-validate-pred email-address?
  "Returns true if the email address is valid according to the validation
  grammar defined in the
  [HTML specification](https://html.spec.whatwg.org/#valid-e-mail-address)
  for an input of type email, otherwise returns false. The domain of the email
  address is not checked for validity."
  [value]
  {:requirement :must-be-an-email-address
   :gen         dt-email-gen/gen-email-address}
  (dt-email/email-address? value))
