(ns datatype.bool.core
  (:require
   [datatype.support :as dts]))

(defn boolean-string? [value]
  (dts/exception->false
    (and (string? value)
      (boolean?
        (Boolean/valueOf ^String value)))))
