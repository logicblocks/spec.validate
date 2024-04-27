(ns spec.definition.number.core
  (:refer-clojure :exclude [zero? integer?])
  (:require
   [clojure.spec.gen.alpha :as gen]

   [datatype.number.core :as dt-number]
   [datatype.number.gen :as dt-number-gen]

   [spec.definition.core :as sd]

   [spec.validate.core :as sv-core]))

(declare
  integer?
  integer-string?

  floating-point-number?

  ; int?
  ; float?
  ; double?
  ; decimal?

  decimal-string?

  ; number-string?

  positive?
  ; positive-integer-string?
  ; positive-decimal-string?
  ; positive-number-string?

  negative?
  ; negative-integer-string?
  ; negative-decimal-string?
  ; negative-number-string?

  ; natural-number?
  ; natural-number-string?

  zero?
  ; zero-string?
  )

(sd/def-validate-pred integer?
  "Returns true if the provided value is an integer, else returns false."
  [value]
  {:requirement :must-be-an-integer
   :gen         #(gen/gen-for-pred clojure.core/integer?)}
  (clojure.core/integer? value))

(defmethod sv-core/pred-requirement 'clojure.core/integer?
  [_]
  :must-be-an-integer)

(sd/def-validate-pred integer-string?
  "Returns true if the provided value is a string representing a base 10
  integer, else returns false."
  [value]
  {:requirement :must-be-an-integer-string
   :gen         dt-number-gen/gen-integer-string}
  (dt-number/integer-string? value))

(sd/def-validate-pred floating-point-number?
  "Returns true if the provided value is a floating point number (Float or
  Double), else returns false."
  [value]
  {:requirement :must-be-a-floating-point-number
   :gen         #(gen/gen-for-pred clojure.core/float?)}
  (sd/exception->false
    (clojure.core/float? value)))

(sd/def-validate-pred decimal-string?
  "Returns true if the provided value is a string representing a decimal number,
  else returns false."
  [value]
  {:requirement :must-be-a-decimal-string
   :gen         dt-number-gen/gen-decimal-string}
  (dt-number/decimal-string? value))

(sd/def-validate-pred positive?
  "Returns true if the provided value is a positive number, else returns false."
  [value]
  {:requirement :must-be-a-positive-number
   :gen         #(gen/gen-for-pred pos-int?)}
  (sd/exception->false
    (clojure.core/pos? value)))

(defmethod sv-core/pred-requirement 'clojure.core/pos?
  [_]
  :must-be-a-positive-number)

(defmethod sv-core/pred-requirement 'clojure.core/pos-int?
  [_]
  :must-be-a-positive-integer)

(sd/def-validate-pred negative?
  "Returns true if the provided value is a negative number, else returns false."
  [value]
  {:requirement :must-be-a-negative-number
   :gen         #(gen/gen-for-pred neg-int?)}
  (sd/exception->false
    (clojure.core/neg? value)))

(defmethod sv-core/pred-requirement 'clojure.core/neg?
  [_]
  :must-be-a-negative-number)

(defmethod sv-core/pred-requirement 'clojure.core/neg-int?
  [_]
  :must-be-a-negative-integer)

(sd/def-validate-pred zero?
  "Returns true if the provided value is zero, else returns false."
  [value]
  {:requirement :must-be-zero
   :gen         #(gen/gen-for-pred clojure.core/zero?)}
  (sd/exception->false
    (clojure.core/zero? value)))

(defmethod sv-core/pred-requirement 'clojure.core/zero?
  [_]
  :must-be-zero)
