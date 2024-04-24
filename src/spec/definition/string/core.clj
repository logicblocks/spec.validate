(ns spec.definition.string.core
  (:refer-clojure :exclude [string?])
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [icu4clj.text.unicode-set :as icu-tus]
   [icu4clj.text.unicode-set-patterns :as icu-tusp]

   [datatype.string.core :as dt-string]

   [spec.definition.core :as sd]))

(defn gen-character-string-unicode [pattern]
  (let [ranges (icu-tus/codepoint-ranges pattern)
        generators (map #(apply gen/choose %) ranges)]
    (gen/fmap
      (fn [^long i] (Character/toString i))
      (apply gen/one-of [generators]))))

(defn gen-string-unicode
  ([pattern]
   (gen/fmap string/join
     (gen/vector (gen-character-string-unicode pattern))))
  ([pattern {:keys [min-length max-length]}]
   (assert
     (and
       (not (nil? min-length))
       (not (nil? max-length)))
     "Both `min-length` and `max-length` must be provided")
   (gen/fmap string/join
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
   :gen         #(gen-string-unicode icu-tusp/whitespace-pattern)}
  (dt-string/blank? value))

(sd/def-validate-pred not-blank?
  "Returns true if the provided value is a string containing non-whitespace
  characters, else returns false."
  [value]
  {:requirement :must-be-a-non-blank-string
   :gen         #(gen-string-unicode icu-tusp/non-whitespace-pattern)}
  (dt-string/not-blank? value))

(sd/def-validate-pred ascii-digits?
  "Returns true if the provided value is a string containing zero or more
  characters in the set [0-9], else returns false."
  [value]
  {:requirement :must-be-a-string-of-ascii-digits
   :gen         #(gen-string-unicode icu-tusp/ascii-digit-pattern)}
  (dt-string/ascii-digits? value))

(sd/def-validate-pred lowercase-ascii-alphabetics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-z], else returns false."
  [value]
  {:requirement :must-be-a-string-of-lowercase-ascii-alphabetic-characters
   :gen         #(gen-string-unicode
                   icu-tusp/lowercase-ascii-alphabetic-pattern)}
  (dt-string/lowercase-ascii-alphabetics? value))

(sd/def-validate-pred uppercase-ascii-alphabetics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [A-Z], else returns false."
  [value]
  {:requirement :must-be-a-string-of-uppercase-ascii-alphabetic-characters
   :gen         #(gen-string-unicode
                   icu-tusp/uppercase-ascii-alphabetic-pattern)}
  (dt-string/uppercase-ascii-alphabetics? value))

(sd/def-validate-pred ascii-alphabetics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-zA-Z], else returns false."
  [value]
  {:requirement :must-be-a-string-of-ascii-alphabetic-characters
   :gen         #(gen-string-unicode icu-tusp/ascii-alphabetic-pattern)}
  (dt-string/ascii-alphabetics? value))

(sd/def-validate-pred lowercase-ascii-alphanumerics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-z0-9], else returns false."
  [value]
  {:requirement :must-be-a-string-of-lowercase-ascii-alphanumeric-characters
   :gen         #(gen-string-unicode
                   icu-tusp/lowercase-ascii-alphanumeric-pattern)}
  (dt-string/lowercase-ascii-alphanumerics? value))

(sd/def-validate-pred uppercase-ascii-alphanumerics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [A-Z0-9], else returns false."
  [value]
  {:requirement :must-be-a-string-of-uppercase-ascii-alphanumeric-characters
   :gen         #(gen-string-unicode
                   icu-tusp/uppercase-ascii-alphanumeric-pattern)}
  (dt-string/uppercase-ascii-alphanumerics? value))

(sd/def-validate-pred ascii-alphanumerics?
  "Returns true if the provided value is a string containing one or more
  characters in the set [a-zA-Z0-9], else returns false."
  [value]
  {:requirement :must-be-a-string-of-ascii-alphanumeric-characters
   :gen         #(gen-string-unicode
                   icu-tusp/ascii-alphanumeric-pattern)}
  (dt-string/ascii-alphanumerics? value))
