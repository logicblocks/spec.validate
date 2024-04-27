(ns datatype.bool.core
  (:require
   [datatype.support :as dts]))

(defn boolean-string?
  "Returns true if the provided value is a string representing a boolean value
  (\"true\" or \"false\", case-insensitive), else returns false."
  [value]
  (dts/exception->false
    (and (string? value)
      (boolean?
        (Boolean/valueOf ^String value)))))
