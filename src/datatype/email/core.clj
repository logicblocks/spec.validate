(ns datatype.email.core
  (:require
   [datatype.support :as dts]))

(def email-address-pattern
  (re-pattern
    (str
      "[a-zA-Z0-9\\.!#$%&'*+\\/=?^_`{|}~-]+"
      "@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
      "(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*")))

(defn email-address?
  "Returns true if the email address is valid according to the validation
  grammar defined in the
  [HTML specification](https://html.spec.whatwg.org/#valid-e-mail-address)
  for an input of type email, otherwise returns false. The domain of the email
  address is not checked for validity."
  [value]
  (dts/exception->false
    (dts/re-satisfies?
      (dts/re-exact-pattern email-address-pattern)
      value)))
