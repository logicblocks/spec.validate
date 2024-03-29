(ns spec.validate.currency
  (:require
   [clojurewerkz.money.amounts :as money-amounts]
   [clojurewerkz.money.currencies :as money-currencies]

   [spec.validate.core :as sv-core]
   [spec.validate.utils :as sv-utils]))

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
