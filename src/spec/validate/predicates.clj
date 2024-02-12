(ns spec.validate.predicates
  (:refer-clojure :exclude [uuid? zero? string? boolean? integer?])
  (:require
   [clojure.string :as string]

   [clj-time.format :as datetimes]

   [valip.predicates :as valip-predicates]

   [clojurewerkz.money.amounts :as money-amounts]
   [clojurewerkz.money.currencies :as money-currencies]

   [org.bovinegenius.exploding-fish :as urls]

   [spec.validate.core :as sv-core]
   [spec.validate.utils :as sv-utils])
  (:import
   [com.google.i18n.phonenumbers PhoneNumberUtil NumberParseException]
   [org.apache.commons.validator.routines DoubleValidator]))

;; identifiers
(def ^:private uuid-v4-regex
  (re-pattern
    (str "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-4[a-fA-F0-9]{3}-"
      "[89abAB][a-fA-F0-9]{3}-[a-fA-F0-9]{12}$")))

(defn uuid-v4?
  "Returns true if the provided value is a string representing a v4 UUID,
  else returns false."
  [value]
  (sv-utils/exception->false (boolean (re-matches uuid-v4-regex value))))

(defmethod sv-core/pred-requirement 'spec.validate.predicates/uuid-v4?
  [_]
  :must-be-a-v4-uuid)

;; URLs
(defn absolute-url?
  "Returns true if the provided value is a string representing an absolute URL,
  else returns false."
  [value]
  (sv-utils/exception->false (urls/absolute? value)))

(defmethod sv-core/pred-requirement 'spec.validate.predicates/absolute-url?
  [_]
  :must-be-an-absolute-url)

;; strings

;; boolean
(defn boolean?
  "Returns true if the provided value is a boolean, else returns false."
  [value]
  (clojure.core/boolean? value))

(defmethod sv-core/pred-requirement 'spec.validate.predicates/boolean?
  [_]
  :must-be-a-boolean)

;; numbers
(defn- parse-number [value]
  (if (clojure.core/string? value)
    (.validate (DoubleValidator.) value)
    value))

(defn integer?
  "Returns true if the provided value is an integer, else returns false."
  [value]
  (clojure.core/integer? value))

(defmethod sv-core/pred-requirement 'clojure.core/integer?
  [_]
  :must-be-an-integer)

(defn positive?
  "Returns true if the provided value is a positive number or a string
  representing a positive number, else returns false."
  [value]
  (sv-utils/exception->false ((valip.predicates/gt 0) value)))

(defmethod sv-core/pred-requirement 'spec.validate.predicates/positive?
  [_]
  :must-be-a-positive-number)

(defn negative?
  "Returns true if the provided value is a positive number or a string
  representing a positive number, else returns false."
  [value]
  (sv-utils/exception->false ((valip.predicates/lt 0) value)))

(defmethod sv-core/pred-requirement 'spec.validate.predicates/negative?
  [_]
  :must-be-a-negative-number)

(defn zero?
  "Returns true if the provided value is zero or a string representing zero,
  else returns false."
  [value]
  (sv-utils/exception->false (clojure.core/zero? (parse-number value))))

(defmethod sv-core/pred-requirement 'spec.validate.predicates/zero?
  [_]
  :must-be-zero)

;; collections
(defn not-empty?
  "Returns true if the provided value has a count of at least 1,
  else returns false."
  [value]
  (sv-utils/exception->false (>= (count value) 1)))

(defmethod sv-core/pred-requirement 'spec.validate.predicates/not-empty?
  [_]
  :must-not-be-empty)

(defn length-equal-to?
  "Returns a function that returns true if the provided value has the
  specified length, else returns false."
  [length]
  (fn [value]
    (sv-utils/exception->false (= (count value) length))))

(def length-equal-to-1? (length-equal-to? 1))
(def length-equal-to-2? (length-equal-to? 2))
(def length-equal-to-3? (length-equal-to? 3))
(def length-equal-to-4? (length-equal-to? 4))
(def length-equal-to-5? (length-equal-to? 5))
(def length-equal-to-6? (length-equal-to? 6))
(def length-equal-to-7? (length-equal-to? 7))
(def length-equal-to-8? (length-equal-to? 8))
(def length-equal-to-9? (length-equal-to? 9))
(def length-equal-to-10? (length-equal-to? 10))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-1?
  [_]
  :must-have-length-equal-to-1)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-2?
  [_]
  :must-have-length-equal-to-2)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-3?
  [_]
  :must-have-length-equal-to-3)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-4?
  [_]
  :must-have-length-equal-to-4)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-5?
  [_]
  :must-have-length-equal-to-5)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-6?
  [_]
  :must-have-length-equal-to-6)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-7?
  [_]
  :must-have-length-equal-to-7)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-8?
  [_]
  :must-have-length-equal-to-8)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-9?
  [_]
  :must-have-length-equal-to-9)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-10?
  [_]
  :must-have-length-equal-to-10)

(defn length-less-than?
  "Returns a function that returns true if the provided value has a length
  less than the specified length, else returns false."
  [length]
  (fn [value]
    (sv-utils/exception->false (and (some? value) (< (count value) length)))))

(def length-less-than-1? (length-less-than? 1))
(def length-less-than-2? (length-less-than? 2))
(def length-less-than-3? (length-less-than? 3))
(def length-less-than-4? (length-less-than? 4))
(def length-less-than-5? (length-less-than? 5))
(def length-less-than-6? (length-less-than? 6))
(def length-less-than-7? (length-less-than? 7))
(def length-less-than-8? (length-less-than? 8))
(def length-less-than-9? (length-less-than? 9))
(def length-less-than-10? (length-less-than? 10))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-1?
  [_]
  :must-have-length-less-than-1)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-2?
  [_]
  :must-have-length-less-than-2)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-3?
  [_]
  :must-have-length-less-than-3)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-4?
  [_]
  :must-have-length-less-than-4)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-5?
  [_]
  :must-have-length-less-than-5)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-6?
  [_]
  :must-have-length-less-than-6)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-7?
  [_]
  :must-have-length-less-than-7)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-8?
  [_]
  :must-have-length-less-than-8)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-9?
  [_]
  :must-have-length-less-than-9)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-10?
  [_]
  :must-have-length-less-than-10)

(defn length-greater-than?
  "Returns a function that returns true if the provided value has a length
  greater than the specified length, else returns false."
  [length]
  (fn [value]
    (sv-utils/exception->false (and (some? value) (> (count value) length)))))

(def length-greater-than-1? (length-greater-than? 1))
(def length-greater-than-2? (length-greater-than? 2))
(def length-greater-than-3? (length-greater-than? 3))
(def length-greater-than-4? (length-greater-than? 4))
(def length-greater-than-5? (length-greater-than? 5))
(def length-greater-than-6? (length-greater-than? 6))
(def length-greater-than-7? (length-greater-than? 7))
(def length-greater-than-8? (length-greater-than? 8))
(def length-greater-than-9? (length-greater-than? 9))
(def length-greater-than-10? (length-greater-than? 10))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-1?
  [_]
  :must-have-length-greater-than-1)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-2?
  [_]
  :must-have-length-greater-than-2)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-3?
  [_]
  :must-have-length-greater-than-3)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-4?
  [_]
  :must-have-length-greater-than-4)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-5?
  [_]
  :must-have-length-greater-than-5)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-6?
  [_]
  :must-have-length-greater-than-6)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-7?
  [_]
  :must-have-length-greater-than-7)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-8?
  [_]
  :must-have-length-greater-than-8)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-9?
  [_]
  :must-have-length-greater-than-9)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-10?
  [_]
  :must-have-length-greater-than-10)

;; dates
(defn iso8601-datetime?
  "Returns true if the provided value is a string representing an ISO8601
  datetime, else returns false."
  [value]
  (nil->false (and (datetimes/parse value) true)))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/iso8601-datetime?
  [_]
  :must-be-an-iso8601-datetime)

;; currency
(defn currency-amount?
  "Returns true if the provided value is a string representing a currency
  amount, else returns false."
  [value]
  (sv-utils/exception->false (and (money-amounts/parse (str "GBP " value)) true)))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/currency-amount?
  [_]
  :must-be-a-currency-amount)

(defn currency-code?
  "Returns true if the provided value is a string representing a currency
  code, else returns false."
  [value]
  (sv-utils/exception->false (and (money-currencies/for-code value) true)))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/currency-code?
  [_]
  :must-be-a-currency-code)

;; post codes
(def ^:private postcode-regex
  (re-pattern
    (str "^([A-Za-z][A-Ha-hK-Yk-y]?[0-9][A-Za-z0-9]? "
      "[0-9][A-Za-z]{2}|[Gg][Ii][Rr] 0[Aa]{2})$")))

(defn postcode?
  "Returns true if the provided value is a string representing a UK postcode,
  else returns false."
  [value]
  (sv-utils/exception->false (boolean (re-matches postcode-regex value))))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/postcode?
  [_]
  :must-be-a-uk-postcode)

;; phone numbers
(def ^:dynamic *default-phone-number-region-code* "GB")

(def ^:private ^PhoneNumberUtil phone-number-util
  (PhoneNumberUtil/getInstance))

(defn- string->PhoneNumber [value]
  (try
    (.parse phone-number-util value *default-phone-number-region-code*)
    (catch NumberParseException _
      nil)))

(defn phone-number?
  "Returns true if the provided value is a string representing a phone number,
  else returns false. By default, treats phone numbers as being from
  Great Britain, however the default region can be overridden with
  `*default-phone-number-region-code*`."
  [value]
  (sv-utils/exception->false
    (.isValidNumber phone-number-util (string->PhoneNumber value))))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/phone-number?
  [_]
  :must-be-a-phone-number)

;; email address
(defn email-address?
  "Returns true if the email address is valid, based on RFC 2822. Email
  addresses containing quotation marks or square brackets are considered
  invalid, as this syntax is not commonly supported in practise. The domain of
  the email address is not checked for validity."
  [value]
  (sv-utils/exception->false (valip-predicates/email-address? value)))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/email-address?
  [_]
  :must-be-an-email-address)
