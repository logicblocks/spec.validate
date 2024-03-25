(ns spec.validate.unicode
  (:require
    [icu4clj.text.unicode-set :as icu-us]))

(defn unicode-characters [^String pattern]
  (icu-us/character-vector (icu-us/unicode-set pattern)))

(defn unicode-codepoint-ranges [^String pattern]
  (icu-us/codepoint-ranges (icu-us/unicode-set pattern)))

(defn unicode-codepoint-seq [^String value]
  (iterator-seq (.iterator (.codePoints value))))

(defn complement-pattern [pattern]
  (str "[^" pattern "]"))


(def horizontal-tabulation-character
  "\u005Ct")
(def line-feed-character
  "\u005Cn")
(def vertical-tabulation-character
  "\u005Cu000B")
(def form-feed-character
  "\u005Cf")
(def carriage-return-character
  "\u005Cr")
(def file-separator-character
  "\u005Cu001C")
(def group-separator-character
  "\u005Cu001D")
(def record-separator-character
  "\u005Cu001E")
(def unit-separator-character
  "\u005Cu001F")

(def no-break-space-character
  "\u005Cu00A0")
(def figure-space-character
  "\u005Cu2007")
(def narrow-no-break-space
  "\u005Cu202F")

(def non-breaking-space-pattern
  (str
    "["
    no-break-space-character
    figure-space-character
    narrow-no-break-space
    "]"))

(def space-separator-pattern
  "[:Zs:]")
(def line-separator-pattern
  "[:Zl:]")
(def paragraph-separator-pattern
  "[:Zp:]")

(def other-whitespace-pattern
  (str
    "["
    horizontal-tabulation-character
    line-feed-character
    vertical-tabulation-character
    form-feed-character
    carriage-return-character
    file-separator-character
    group-separator-character
    record-separator-character
    unit-separator-character
    "]"))

(def whitespace-pattern
  (str
    "["
    "["
    space-separator-pattern
    line-separator-pattern
    paragraph-separator-pattern
    other-whitespace-pattern
    "] - "
    non-breaking-space-pattern
    "]"))

(def non-whitespace-pattern
  (complement-pattern whitespace-pattern))

(def ascii-digit-pattern
  "[0-9]")

(def non-ascii-digit-pattern
  (complement-pattern ascii-digit-pattern))

(def lowercase-ascii-alphabetic-pattern
  "[a-z]")

(def non-lowercase-ascii-alphabetic-pattern
  (complement-pattern lowercase-ascii-alphabetic-pattern))

(def uppercase-ascii-alphabetic-pattern
  "[A-Z]")

(def non-uppercase-ascii-alphabetic-pattern
  (complement-pattern uppercase-ascii-alphabetic-pattern))

(def ascii-alphabetic-pattern
  (str
    "["
    lowercase-ascii-alphabetic-pattern
    uppercase-ascii-alphabetic-pattern
    "]"))

(def non-ascii-alphabetic-pattern
  (complement-pattern ascii-alphabetic-pattern))

(def lowercase-ascii-alphanumeric-pattern
  (str
    "["
    ascii-digit-pattern
    lowercase-ascii-alphabetic-pattern
    "]"))

(def non-lowercase-ascii-alphanumeric-pattern
  (complement-pattern lowercase-ascii-alphanumeric-pattern))

(def uppercase-ascii-alphanumeric-pattern
  (str
    "["
    ascii-digit-pattern
    uppercase-ascii-alphabetic-pattern
    "]"))

(def non-uppercase-ascii-alphanumeric-pattern
  (complement-pattern uppercase-ascii-alphanumeric-pattern))

(def ascii-alphanumeric-pattern
  (str
    "["
    ascii-digit-pattern
    lowercase-ascii-alphabetic-pattern
    uppercase-ascii-alphabetic-pattern
    "]"))

(def non-ascii-alphanumeric-pattern
  (complement-pattern ascii-alphanumeric-pattern))

(def whitespace-characters
  (unicode-characters whitespace-pattern))

(def non-whitespace-characters
  (unicode-characters non-whitespace-pattern))

(def ascii-digit-characters
  (unicode-characters ascii-digit-pattern))

(def non-ascii-digit-characters
  (unicode-characters non-ascii-digit-pattern))

(def lowercase-ascii-alphabetic-characters
  (unicode-characters lowercase-ascii-alphabetic-pattern))

(def non-lowercase-ascii-alphabetic-characters
  (unicode-characters non-lowercase-ascii-alphabetic-pattern))

(def uppercase-ascii-alphabetic-characters
  (unicode-characters uppercase-ascii-alphabetic-pattern))

(def non-uppercase-ascii-alphabetic-characters
  (unicode-characters non-uppercase-ascii-alphabetic-pattern))

(def ascii-alphabetic-characters
  (unicode-characters ascii-alphabetic-pattern))

(def non-ascii-alphabetic-characters
  (unicode-characters non-ascii-alphabetic-pattern))

(def lowercase-ascii-alphanumeric-characters
  (unicode-characters lowercase-ascii-alphanumeric-pattern))

(def non-lowercase-ascii-alphanumeric-characters
  (unicode-characters non-lowercase-ascii-alphanumeric-pattern))

(def uppercase-ascii-alphanumeric-characters
  (unicode-characters uppercase-ascii-alphanumeric-pattern))

(def non-uppercase-ascii-alphanumeric-characters
  (unicode-characters non-uppercase-ascii-alphanumeric-pattern))

(def ascii-alphanumeric-characters
  (unicode-characters ascii-alphanumeric-pattern))

(def non-ascii-alphanumeric-characters
  (unicode-characters non-ascii-alphanumeric-pattern))
