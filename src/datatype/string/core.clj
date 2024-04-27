(ns datatype.string.core
  (:require
   [clojure.string :as string]

   [datatype.support :as dts]))

(defn blank?
  "Returns true if the provided value is an empty string or a string containing
  only whitespace characters, else returns false."
  [value]
  (dts/exception->false
    (and
      (not (nil? value))
      (string/blank? value))))

(defn not-blank?
  "Returns true if the provided value is a string containing non-whitespace
  characters, else returns false."
  [value]
  (dts/exception->false (not (string/blank? value))))

(defn ascii-digits?
  "Returns true if the provided value is a string containing zero or more
  characters in the set [0-9], else returns false."
  [value]
  (dts/exception->false
    (dts/re-satisfies? #"^\d+$" value)))

(defn lowercase-ascii-alphabetics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-z], else returns false."
  [value]
  (dts/exception->false
    (dts/re-satisfies? #"^[a-z]+$" value)))

(defn uppercase-ascii-alphabetics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [A-Z], else returns false."
  [value]
  (dts/exception->false
    (dts/re-satisfies? #"^[A-Z]+$" value)))

(defn ascii-alphabetics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-zA-Z], else returns false."
  [value]
  (dts/exception->false
    (dts/re-satisfies? #"^[a-zA-Z]+$" value)))

(defn lowercase-ascii-alphanumerics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-z0-9], else returns false."
  [value]
  (dts/exception->false
    (dts/re-satisfies? #"^[a-z0-9]+$" value)))

(defn uppercase-ascii-alphanumerics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [A-Z0-9], else returns false."
  [value]
  (dts/exception->false
    (dts/re-satisfies? #"^[A-Z0-9]+$" value)))

(defn ascii-alphanumerics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-zA-Z0-9], else returns false."
  [value]
  (dts/exception->false
    (dts/re-satisfies? #"^[a-zA-Z0-9]+$" value)))
