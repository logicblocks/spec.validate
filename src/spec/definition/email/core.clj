(ns spec.definition.email.core
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [datatype.email.core :as dt-email]

   [spec.definition.core :as sd-def]
   [spec.definition.string.core :as sd-string]
   [spec.definition.domain.core :as sd-domain]))

(declare
  email-address?)

(defn gen-email-address []
  (gen/fmap
    (fn [[local-part domain-part]]
      (str local-part "@" domain-part))
    (gen/tuple
      (gen/fmap
        string/join
        (gen/vector
          (gen/frequency
            [[1 (sd-string/gen-character-string-unicode
                  "[\\^\\&.!#$%'*+/\\=?_`{|}~-]")]
             [9 (sd-string/gen-character-string-unicode
                  "[a-zA-Z0-9]")]])
          1 63))
      (sd-domain/gen-domain-name))))

(sd-def/def-validate-pred email-address?
  "Returns true if the email address is valid according to the validation
  grammar defined in the
  [HTML specification](https://html.spec.whatwg.org/#valid-e-mail-address)
  for an input of type email, otherwise returns false. The domain of the email
  address is not checked for validity."
  [value]
  {:requirement :must-be-an-email-address
   :gen         gen-email-address}
  (dt-email/email-address? value))
