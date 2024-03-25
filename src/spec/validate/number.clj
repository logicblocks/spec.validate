(ns spec.validate.number
  (:refer-clojure :exclude [zero? integer?])
  (:require
    [spec.validate.core :as sv-core]
    [spec.validate.utils :as sv-utils])
  (:import
    [org.apache.commons.validator.routines DoubleValidator]))

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
