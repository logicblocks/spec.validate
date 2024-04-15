(ns spec.validate.string
  (:refer-clojure :exclude [string?])
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [spec.validate.unicode :as sv-unicode]
   [spec.validate.utils :as sv-utils]))

(defn gen-character-string-unicode [pattern]
  (let [ranges (sv-unicode/unicode-codepoint-ranges pattern)
        generators (map #(apply gen/choose %) ranges)]
    (gen/fmap
      (fn [^long i] (Character/toString i))
      (apply gen/one-of [generators]))))

(defn gen-string-unicode
  ([pattern]
   (gen/fmap clojure.string/join
     (gen/vector (gen-character-string-unicode pattern))))
  ([pattern {:keys [min-length max-length]}]
   (assert
     (and
       (not (nil? min-length))
       (not (nil? max-length)))
     "Both `min-length` and `max-length` must be provided")
   (gen/fmap clojure.string/join
     (gen/vector (gen-character-string-unicode pattern)
       min-length max-length))))

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

(sv-utils/extend-pred-with-requirement
  'clojure.core/string?
  :must-be-a-string)

(sv-utils/def-validate-pred string?
  "Returns true if the provided value is a string, else returns false."
  [value]
  {:requirement :must-be-a-string
   :gen         #(gen/gen-for-pred clojure.core/string?)}
  (clojure.core/string? value))

(sv-utils/def-validate-pred blank?
  "Returns true if the provided value is an empty string or a string containing
  only whitespace characters, else returns false."
  [value]
  {:requirement :must-be-a-blank-string
   :gen         #(gen-string-unicode sv-unicode/whitespace-pattern)}
  (sv-utils/exception->false (and (not (nil? value)) (string/blank? value))))

(sv-utils/def-validate-pred not-blank?
  "Returns true if the provided value is a string containing non-whitespace
  characters, else returns false."
  [value]
  {:requirement :must-be-a-non-blank-string
   :gen         #(gen-string-unicode sv-unicode/non-whitespace-pattern)}
  (sv-utils/exception->false (not (string/blank? value))))

(sv-utils/def-validate-pred ascii-digits?
  "Returns true if the provided value is a string containing zero or more
  characters in the set [0-9], else returns false."
  [value]
  {:requirement :must-be-a-string-of-ascii-digits
   :gen         #(gen-string-unicode sv-unicode/ascii-digit-pattern)}
  (sv-utils/exception->false (boolean (re-matches #"^\d+$" value))))

(sv-utils/def-validate-pred lowercase-ascii-alphabetics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-z], else returns false."
  [value]
  {:requirement :must-be-a-string-of-lowercase-ascii-alphabetic-characters
   :gen         #(gen-string-unicode
                   sv-unicode/lowercase-ascii-alphabetic-pattern)}
  (sv-utils/exception->false (boolean (re-matches #"^[a-z]+$" value))))

(sv-utils/def-validate-pred uppercase-ascii-alphabetics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [A-Z], else returns false."
  [value]
  {:requirement :must-be-a-string-of-uppercase-ascii-alphabetic-characters
   :gen         #(gen-string-unicode
                   sv-unicode/uppercase-ascii-alphabetic-pattern)}
  (sv-utils/exception->false (boolean (re-matches #"^[A-Z]+$" value))))

(sv-utils/def-validate-pred ascii-alphabetics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-zA-Z], else returns false."
  [value]
  {:requirement :must-be-a-string-of-ascii-alphabetic-characters
   :gen         #(gen-string-unicode sv-unicode/ascii-alphabetic-pattern)}
  (sv-utils/exception->false (boolean (re-matches #"^[a-zA-Z]+$" value))))

(sv-utils/def-validate-pred lowercase-ascii-alphanumerics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-z0-9], else returns false."
  [value]
  {:requirement :must-be-a-string-of-lowercase-ascii-alphanumeric-characters
   :gen         #(gen-string-unicode
                   sv-unicode/lowercase-ascii-alphanumeric-pattern)}
  (sv-utils/exception->false (boolean (re-matches #"^[a-z0-9]+$" value))))

(sv-utils/def-validate-pred uppercase-ascii-alphanumerics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [A-Z0-9], else returns false."
  [value]
  {:requirement :must-be-a-string-of-uppercase-ascii-alphanumeric-characters
   :gen         #(gen-string-unicode
                   sv-unicode/uppercase-ascii-alphanumeric-pattern)}
  (sv-utils/exception->false (boolean (re-matches #"^[A-Z0-9]+$" value))))

(sv-utils/def-validate-pred ascii-alphanumerics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-zA-Z0-9], else returns false."
  [value]
  {:requirement :must-be-a-string-of-ascii-alphanumeric-characters
   :gen         #(gen-string-unicode
                   sv-unicode/ascii-alphanumeric-pattern)}
  (sv-utils/exception->false (boolean (re-matches #"^[a-zA-Z0-9]+$" value))))
