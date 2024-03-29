(ns spec.validate.number
  (:refer-clojure :exclude [zero? integer? decimal?])
  (:require
   [clojure.spec.gen.alpha :as gen]

   [spec.validate.core :as sv-core]
   [spec.validate.utils :as sv-utils]))

(declare
  integer?
  integer-string?

  decimal?
  decimal-string?

  positive?
  positive-integer-string?
  positive-decimal-string?

  negative?
  negative-integer-string?
  negative-decimal-string?

  natural-number?
  natural-number-string?

  zero?
  zero-string?)

(sv-utils/def-validate-pred integer?
  "Returns true if the provided value is an integer, else returns false."
  [value]
  {:requirement :must-be-an-integer
   :gen         #(gen/gen-for-pred clojure.core/integer?)}
  (clojure.core/integer? value))

(defmethod sv-core/pred-requirement 'clojure.core/integer?
  [_]
  :must-be-an-integer)

(sv-utils/def-validate-pred integer-string?
  "Returns true if the provided value is a string representing a base 10
  integer, else returns false."
  [value]
  {:requirement :must-be-an-integer-string
   :gen         #(gen/fmap str
                   (gen/gen-for-pred clojure.core/integer?))}
  (sv-utils/exception->false
    (sv-utils/re-satisfies? #"^[+-]?(0$|[1-9][0-9]*$)" value)))

(defn positive?
  "Returns true if the provided value is a positive number or a string
  representing a positive number, else returns false."
  [value]
  (sv-utils/exception->false
    (clojure.core/pos? (parse-double value))))

(defmethod sv-core/pred-requirement 'spec.validate.predicates/positive?
  [_]
  :must-be-a-positive-number)

(defn negative?
  "Returns true if the provided value is a positive number or a string
  representing a positive number, else returns false."
  [value]
  (sv-utils/exception->false
    (clojure.core/neg? (parse-double value))))

(defmethod sv-core/pred-requirement 'spec.validate.predicates/negative?
  [_]
  :must-be-a-negative-number)

(defn zero?
  "Returns true if the provided value is zero or a string representing zero,
  else returns false."
  [value]
  (sv-utils/exception->false
    (clojure.core/zero? (parse-double value))))

(defmethod sv-core/pred-requirement 'spec.validate.predicates/zero?
  [_]
  :must-be-zero)
