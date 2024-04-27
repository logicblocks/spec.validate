(ns datatype.time.core
  (:require
   [cljc.java-time.offset-date-time :as jt-odt]

   [datatype.support :as dts]))

(defn iso8601-zoned-datetime-string?
  "Returns true if the provided value is a string representing an ISO8601
  zoned datetime, else returns false."
  [value]
  (dts/exception->false
    (boolean (jt-odt/parse value))))
