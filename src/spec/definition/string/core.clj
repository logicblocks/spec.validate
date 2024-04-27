(ns spec.definition.string.core
  (:refer-clojure :exclude [string?])
  (:require
   [clojure.spec.gen.alpha :as gen]

   [datatype.string.core :as dt-string]
   [datatype.string.gen :as dt-string-gen]

   [spec.definition.core :as sd]))

(declare
  string?
  blank?
  not-blank?
  ascii-digits?
  lowercase-ascii-alphabetics?
  uppercase-ascii-alphabetics?
  ascii-alphabetics?
  lowercase-ascii-alphanumerics?
  uppercase-ascii-alphanumerics?
  ascii-alphanumerics?)

(sd/extend-pred-with-requirement
  'clojure.core/string?
  :must-be-a-string)

(sd/def-validate-pred string?
  "Returns true if the provided value is a string, else returns false."
  [value]
  {:requirement :must-be-a-string
   :gen         #(gen/gen-for-pred clojure.core/string?)}
  (clojure.core/string? value))

(sd/def-validate-pred blank?
  "Returns true if the provided value is an empty string or a string containing
  only whitespace characters, else returns false."
  [value]
  {:requirement :must-be-a-blank-string
   :gen         dt-string-gen/gen-string-whitespace}
  (dt-string/blank? value))

(sd/def-validate-pred not-blank?
  "Returns true if the provided value is a string containing non-whitespace
  characters, else returns false."
  [value]
  {:requirement :must-be-a-non-blank-string
   :gen         dt-string-gen/gen-string-non-whitespace}
  (dt-string/not-blank? value))

(sd/def-validate-pred ascii-digits?
  "Returns true if the provided value is a string containing zero or more
  characters in the set [0-9], else returns false."
  [value]
  {:requirement :must-be-a-string-of-ascii-digits
   :gen         dt-string-gen/gen-string-ascii-digits}
  (dt-string/ascii-digits? value))

(sd/def-validate-pred lowercase-ascii-alphabetics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-z], else returns false."
  [value]
  {:requirement :must-be-a-string-of-lowercase-ascii-alphabetic-characters
   :gen         dt-string-gen/gen-string-lowercase-ascii-alphabetics}
  (dt-string/lowercase-ascii-alphabetics? value))

(sd/def-validate-pred uppercase-ascii-alphabetics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [A-Z], else returns false."
  [value]
  {:requirement :must-be-a-string-of-uppercase-ascii-alphabetic-characters
   :gen         dt-string-gen/gen-string-uppercase-ascii-alphabetics}
  (dt-string/uppercase-ascii-alphabetics? value))

(sd/def-validate-pred ascii-alphabetics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-zA-Z], else returns false."
  [value]
  {:requirement :must-be-a-string-of-ascii-alphabetic-characters
   :gen         dt-string-gen/gen-string-ascii-alphabetics}
  (dt-string/ascii-alphabetics? value))

(sd/def-validate-pred lowercase-ascii-alphanumerics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-z0-9], else returns false."
  [value]
  {:requirement :must-be-a-string-of-lowercase-ascii-alphanumeric-characters
   :gen         dt-string-gen/gen-string-lowercase-ascii-alphanumerics}
  (dt-string/lowercase-ascii-alphanumerics? value))

(sd/def-validate-pred uppercase-ascii-alphanumerics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [A-Z0-9], else returns false."
  [value]
  {:requirement :must-be-a-string-of-uppercase-ascii-alphanumeric-characters
   :gen         dt-string-gen/gen-string-uppercase-ascii-alphanumerics}
  (dt-string/uppercase-ascii-alphanumerics? value))

(sd/def-validate-pred ascii-alphanumerics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-zA-Z0-9], else returns false."
  [value]
  {:requirement :must-be-a-string-of-ascii-alphanumeric-characters
   :gen         dt-string-gen/gen-string-ascii-alphanumerics}
  (dt-string/ascii-alphanumerics? value))
