(ns spec.validate.email
  (:require
   [clojure.spec.gen.alpha :as gen]

   [clojure.string :as string]
   [spec.validate.string :as sv-string]
   [spec.validate.domain :as sv-domain]
   [spec.validate.utils :as sv-utils]))

(declare
  email-address?)

(def ^:private email-address-pattern
  (re-pattern
    (str
      "^[a-zA-Z0-9\\.!#$%&'*+\\/=?^_`{|}~-]+"
      "@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
      "(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$")))

(defn gen-email-address []
  (gen/fmap
    (fn [[local-part domain-part]]
      (str local-part "@" domain-part))
    (gen/tuple
      (gen/fmap
        string/join
        (gen/vector
          (gen/frequency
            [[1 (sv-string/gen-character-string-unicode
                  "[\\^\\&.!#$%'*+/\\=?_`{|}~-]")]
             [9 (sv-string/gen-character-string-unicode
                  "[a-zA-Z0-9]")]])
          1 63))
      (sv-domain/gen-domain-name))))

(sv-utils/def-validate-pred email-address?
  "Returns true if the email address is valid according to the validation
  grammar defined in the
  [HTML specification](https://html.spec.whatwg.org/#valid-e-mail-address)
  for an input of type email, otherwise returns false. The domain of the email
  address is not checked for validity."
  [value]
  {:requirement :must-be-an-email-address
   :gen         gen-email-address}
  (sv-utils/exception->false
    (sv-utils/re-satisfies? email-address-pattern value)))
