(ns datatype.email.core
  (:require
   [datatype.support :as dts]))

(def email-address-pattern
  (re-pattern
    (str
      "[a-zA-Z0-9\\.!#$%&'*+\\/=?^_`{|}~-]+"
      "@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
      "(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*")))

(defn email-address? [value]
  (dts/exception->false
    (dts/re-satisfies?
      (dts/re-exact-pattern email-address-pattern)
      value)))
