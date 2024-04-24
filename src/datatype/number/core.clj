(ns datatype.number.core
  (:require
   [clojure.string :as string]

   [datatype.support :as dts]

   [icu4clj.text.number-format :as icu-tnf]
   [icu4clj.text.unicode-characters :as icu-tuc]))

(defn- grouping-separator-pattern []
  (let [{:keys [grouping-separator]} (icu-tnf/decimal-format-symbols)]
    (if (icu-tuc/space-separator-character? grouping-separator)
      "[\\p{Zs}]"
      (str "[" grouping-separator "]"))))

(defn- sign-pattern []
  (let [{:keys [plus-sign minus-sign]} (icu-tnf/decimal-format-symbols)
        plus-sign (dts/re-quote plus-sign)
        minus-sign (dts/re-quote minus-sign)]
    (str "(" plus-sign "|" minus-sign ")?")))

(defn- non-zero-digits []
  (let [{:keys [digits zero-digit]} (icu-tnf/decimal-format-symbols)]
    (remove #(= zero-digit %) digits)))

(defn integer-string-pattern []
  (let [{:keys [digits zero-digit]} (icu-tnf/decimal-format-symbols)

        zero-pattern
        (str zero-digit)
        integer-pattern
        (str
          "[" (string/join (non-zero-digits)) "]"
          "([" (string/join digits) "]|" (grouping-separator-pattern) ")*")]
    (re-pattern
      (str (sign-pattern) "(" zero-pattern "|" integer-pattern ")"))))

(defn decimal-string-pattern []
  (let [{:keys [digits
                zero-digit
                decimal-separator]}
        (icu-tnf/decimal-format-symbols)

        decimal-separator (dts/re-quote decimal-separator)

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
      (str
        (sign-pattern)
        "("
        zero-pattern "|"
        integer-pattern "|"
        integer-decimal-pattern "|"
        fractional-decimal-pattern
        ")"))))

(defn integer-string? [value]
  (cond
    (nil? value) false
    (not (string? value)) false
    :else
    (dts/exception->false
      (dts/re-satisfies?
        (dts/re-exact-pattern (integer-string-pattern))
        value))))

(defn decimal-string? [value]
  (cond
    (nil? value) false
    (not (string? value)) false
    :else
    (dts/exception->false
      (dts/re-satisfies?
        (dts/re-exact-pattern
          (decimal-string-pattern))
        value))))
