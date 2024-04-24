(ns datatype.address.core
  (:require
   [datatype.support :as dts]))

(def uk-postcode-pattern
  (re-pattern
    (str "[A-Za-z]{1,2}[0-9][A-Za-z0-9]? ?[0-9][A-Za-z]{2}")))

(defn uk-postcode-formatted-string? [value]
  (dts/exception->false
    (dts/re-satisfies?
      (dts/re-exact-pattern uk-postcode-pattern)
      value)))
