(ns datatype.phone.core-test
  (:require
   [clojure.test :refer :all]

   [datatype.phone.core :as dt-phone]

   [spec.test-support.cases :as sv-cases]))

(deftest phone-number-string?
  (sv-cases/assert-cases-satisfied-by dt-phone/phone-number-string?
    (sv-cases/true-case "any non-international GB phone number"
      :samples ["02078910111"
                "07891400100"
                "08001002000"
                "08451234567"
                "0207 891 0111"
                "07891 400 100"
                "0800 100 2000"
                "0845 123 4567"])
    (sv-cases/true-case "any international phone number"
      :samples ["+12126885500"
                "+1-212-688-5500"
                "+5551989063736"
                "+55 (51) 98906 3736"
                "+55-(51)-98906-3736"])
    (sv-cases/false-case "any non-international non-GB phone number"
      :samples ["2126885500"
                "212-688-5500"
                "51989063736"
                "(51) 98906 3736"
                "(51)-98906-3736"])
    (sv-cases/false-case "strings that aren't UUID-like at all"
      :samples ["the quick brown fox jumped over the lazy dog"
                "23.6"
                "true"])
    (sv-cases/false-case "a non-string"
      :samples [true false 35.4 #{"GBP" "USD"}])
    (sv-cases/false-case "nil" :sample nil)))
