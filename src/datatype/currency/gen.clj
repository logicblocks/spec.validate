(ns datatype.currency.gen
  (:require
   [clojure.spec.gen.alpha :as gen]

   [icu4clj.util.currency :as icu-uc]))

(defn gen-currency-code []
  (gen/elements (icu-uc/available-currency-codes)))
