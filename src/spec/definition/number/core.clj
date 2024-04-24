(ns spec.definition.number.core
  (:refer-clojure :exclude [zero? integer?])
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [datatype.number.core :as dt-number]

   [icu4clj.text.number-format :as icu-tnf]

   [spec.definition.core :as sd-def]

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

(sd-def/def-validate-pred integer?
  "Returns true if the provided value is an integer, else returns false."
  [value]
  {:requirement :must-be-an-integer
   :gen         #(gen/gen-for-pred clojure.core/integer?)}
  (clojure.core/integer? value))

(defmethod sv-core/pred-requirement 'clojure.core/integer?
  [_]
  :must-be-an-integer)

(defn gen-integer-string []
  (gen/fmap str
    (gen/gen-for-pred clojure.core/integer?)))

(sd-def/def-validate-pred integer-string?
  "Returns true if the provided value is a string representing a base 10
  integer, else returns false."
  [value]
  {:requirement :must-be-an-integer-string
   :gen         gen-integer-string}
  (dt-number/integer-string? value))

(sd-def/def-validate-pred floating-point-number?
  "Returns true if the provided value is a floating point number (Float or
  Double), else returns false."
  [value]
  {:requirement :must-be-a-floating-point-number
   :gen         #(gen/gen-for-pred clojure.core/float?)}
  (sd-def/exception->false
    (clojure.core/float? value)))

(defn gen-decimal-string []
  (let [{:keys [digits
                zero-digit
                plus-sign
                minus-sign
                decimal-separator]}
        (icu-tnf/decimal-format-symbols)]
    (gen/fmap
      (fn [[sign integer fraction
            include-integer? include-fraction? corrector]]
        (let [include-neither?
              (and
                (not include-integer?)
                (not include-fraction?))
              include-integer? (if include-neither?
                                 (= corrector :integer)
                                 include-integer?)
              include-fraction? (if include-neither?
                                  (= corrector :fraction)
                                  include-fraction?)
              integer
              (if include-integer?
                (string/join integer)
                "")
              fraction
              (if include-fraction?
                (string/join fraction)
                "")]
          (str sign integer decimal-separator fraction)))
      (gen/tuple
        (gen/elements ["" minus-sign plus-sign])
        (gen/one-of
          [(gen/return zero-digit)
           (gen/vector (gen/elements digits) 1 100)])
        (gen/vector (gen/elements digits) 1 100)
        (gen/boolean)
        (gen/boolean)
        (gen/elements [:integer :fraction])))))

(sd-def/def-validate-pred decimal-string?
  "Returns true if the provided value is a string representing a decimal number,
  else returns false."
  [value]
  {:requirement :must-be-a-decimal-string
   :gen         gen-decimal-string}
  (dt-number/decimal-string? value))

(sd-def/def-validate-pred positive?
  "Returns true if the provided value is a positive number, else returns false."
  [value]
  {:requirement :must-be-a-positive-number
   :gen         #(gen/gen-for-pred pos-int?)}
  (sd-def/exception->false
    (clojure.core/pos? value)))

(defmethod sv-core/pred-requirement 'clojure.core/pos?
  [_]
  :must-be-a-positive-number)

(defmethod sv-core/pred-requirement 'clojure.core/pos-int?
  [_]
  :must-be-a-positive-integer)

(sd-def/def-validate-pred negative?
  "Returns true if the provided value is a negative number, else returns false."
  [value]
  {:requirement :must-be-a-negative-number
   :gen         #(gen/gen-for-pred neg-int?)}
  (sd-def/exception->false
    (clojure.core/neg? value)))

(defmethod sv-core/pred-requirement 'clojure.core/neg?
  [_]
  :must-be-a-negative-number)

(defmethod sv-core/pred-requirement 'clojure.core/neg-int?
  [_]
  :must-be-a-negative-integer)

(sd-def/def-validate-pred zero?
  "Returns true if the provided value is zero, else returns false."
  [value]
  {:requirement :must-be-zero
   :gen         #(gen/gen-for-pred clojure.core/zero?)}
  (sd-def/exception->false
    (clojure.core/zero? value)))

(defmethod sv-core/pred-requirement 'clojure.core/zero?
  [_]
  :must-be-zero)
