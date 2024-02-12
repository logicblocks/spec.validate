(ns spec.validate.unicode
  (:import [com.ibm.icu.text UnicodeSet]))

(defn unicode-set [^String pattern]
  (doto (UnicodeSet. pattern)
    (.freeze)))

(defn unicode-characters [^String pattern]
  (vec (unicode-set pattern)))

(defn unicode-codepoint-ranges [^String pattern]
  (into []
    (mapv #(vec [(.codepoint %) (.codepointEnd %)])
      (.ranges (unicode-set pattern)))))

(defn unicode-codepoint-seq [^String value]
  (iterator-seq (.iterator (.codePoints value))))

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
  (str "[^" whitespace-pattern "]"))

(def whitespace-characters
  (unicode-characters whitespace-pattern))

(def non-whitespace-characters
  (unicode-characters non-whitespace-pattern))
