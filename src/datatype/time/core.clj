(ns datatype.time.core
  (:require
   [cljc.java-time.offset-date-time :as jt-odt]

   [datatype.support :as dts]))

(defn iso8601-zoned-datetime-string? [value]
  (dts/exception->false
    (boolean (jt-odt/parse value))))
