(ns spec.validate.number
  (:refer-clojure :exclude [zero? integer? zero?])
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [spec.validate.core :as sv-core]
   [spec.validate.utils :as sv-utils]
   [spec.validate.unicode :as sv-unicode]

   [icu4clj.text.number-format :as icu-tnf]))

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

  ; positive?
  ; positive-integer-string?
  ; positive-decimal-string?
  ; positive-number-string?

  ; negative?
  ; negative-integer-string?
  ; negative-decimal-string?
  ; negative-number-string?

  ; natural-number?
  ; natural-number-string?

  ; zero?
  ; zero-string?
  )

(defn- grouping-separator-pattern []
  (let [{:keys [grouping-separator]} (icu-tnf/decimal-format-symbols)]
    (if (sv-unicode/space-separator-character? grouping-separator)
      "[\\p{Zs}]"
      (str "[" grouping-separator "]"))))

(defn- sign-pattern []
  (let [{:keys [plus-sign minus-sign]} (icu-tnf/decimal-format-symbols)
        plus-sign (sv-utils/re-quote plus-sign)
        minus-sign (sv-utils/re-quote minus-sign)]
    (str "(" plus-sign "|" minus-sign ")?")))

(defn- non-zero-digits []
  (let [{:keys [digits zero-digit]} (icu-tnf/decimal-format-symbols)]
    (remove #(= zero-digit %) digits)))

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
  (cond
    (nil? value) false
    (not (string? value)) false
    :else
    (sv-utils/exception->false
      (let [{:keys [digits zero-digit]} (icu-tnf/decimal-format-symbols)

            zero-pattern
            (str zero-digit)
            integer-pattern
            (str
              "[" (string/join (non-zero-digits)) "]"
              "([" (string/join digits) "]|" (grouping-separator-pattern) ")*")

            pattern
            (re-pattern
              (str
                "^"
                (sign-pattern) "(" zero-pattern "|" integer-pattern ")"
                "$"))]
        (sv-utils/re-satisfies? pattern value)))))

(sv-utils/def-validate-pred floating-point-number?
  "Returns true if the provided value is a floating point number (Float or
  Double), else returns false."
  [value]
  {:requirement :must-be-a-floating-point-number
   :gen         #(gen/gen-for-pred clojure.core/float?)}
  (sv-utils/exception->false
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

(defn decimal-string-pattern []
  (let [{:keys [digits
                zero-digit
                decimal-separator]}
        (icu-tnf/decimal-format-symbols)

        decimal-separator (sv-utils/re-quote decimal-separator)

        fractional-part-pattern (str "[" (string/join digits) "]*")
        zero-pattern (str zero-digit decimal-separator "?")
        integer-pattern
        (str
          "[" (string/join (non-zero-digits)) "]"
          "([" (string/join digits) "]|" (grouping-separator-pattern) ")*"
          decimal-separator "?")
        fractional-decimal-pattern
        (str zero-digit "?" decimal-separator fractional-part-pattern)
        integer-decimal-pattern
        (str integer-pattern decimal-separator fractional-part-pattern)]
    (re-pattern
      (str "^"
        (sign-pattern)
        "("
        zero-pattern "|"
        integer-pattern "|"
        integer-decimal-pattern "|"
        fractional-decimal-pattern
        ")"
        "$"))))

(sv-utils/def-validate-pred decimal-string?
  "Returns true if the provided value is a string representing a decimal number,
  else returns false."
  [value]
  {:requirement :must-be-a-decimal-string
   :gen         gen-decimal-string}
  (cond
    (nil? value) false
    (not (string? value)) false
    :else
    (sv-utils/exception->false
      (sv-utils/re-satisfies? (decimal-string-pattern) value))))

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
