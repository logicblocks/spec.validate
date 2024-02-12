(ns spec.validate.test-support.unicode
  (:require
    [clojure.string :as string]
    [spec.validate.unicode :as uni]))

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

(def lowercase-letter-pattern
  "[:Ll:]")
(def uppercase-letter-pattern
  "[:Lu:]")
(def titlecase-letter-pattern
  "[:Lt:]")
(def modifier-letter-pattern
  "[:Lm:]")

(def decimal-number-pattern
  "[:Nd:]")
(def letter-number-pattern
  "[:Nl:]")
(def other-number-pattern
  "[:No:]")

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

(def letter-pattern
  (str
    "["
    lowercase-letter-pattern
    uppercase-letter-pattern
    titlecase-letter-pattern
    modifier-letter-pattern
    "]"))

(def number-pattern
  (str
    "["
    decimal-number-pattern
    letter-number-pattern
    other-number-pattern
    "]"))

(def whitespace-characters
  (uni/unicode-characters whitespace-pattern))

(def non-whitespace-characters
  (uni/unicode-characters non-whitespace-pattern))

(def letter-characters
  (uni/unicode-characters letter-pattern))

(def number-characters
  (uni/unicode-characters number-pattern))

(let [characters (uni/unicode-characters "[[:blk=ASCII:]-[:gc=Cc:]]")]
  (string/join "\n"
    (map #(string/join " " %)
      (partition 20 20 (repeat " ") characters))))

(range)

(into #{}
  (map #(vec [(.codepoint %) (.codepointEnd %)])
    (.ranges (uni/unicode-set "[[:blk=ASCII:]-[:gc=Cc:]]"))))

(= (into #{} (uni/unicode-characters "[[:blk=ASCII:]-[:gc=Cc:]]"))
  (into #{} (map (comp str char) (range 32 127))))

(into [] (uni/unicode-characters "[[:blk=ASCII:]-[:gc=Cc:]]"))
