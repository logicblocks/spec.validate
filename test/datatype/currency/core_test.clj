(ns datatype.currency.core-test
  (:require
   [clojure.test :refer [deftest]]

   [datatype.currency.core :as dt-currency]

   [spec.test-support.cases :as sv-cases]))

(deftest iso4217-currency-code-string?-as-predicate
  (sv-cases/assert-cases-satisfied-by dt-currency/iso4217-currency-code-string?
    (sv-cases/true-case "any currency code string"
      :samples ["GBP" "BAM" "CUC" "MKN" "BOV" "ARP"])
    (sv-cases/false-case "valid but lowercase currency code strings"
      :samples ["gbp" "bam" "cuc" "mkn" "bov" "arp"])
    (sv-cases/false-case "additional whitespace"
      :samples [" GBP " "\tBAM" " CUC "])
    (sv-cases/false-case "any monetary amount string"
      :samples ["GBP 500.25" "BAM 0.15" "CUC 1000" "MKN -30.28"])
    (sv-cases/false-case
      "3 letter uppercase strings that look like currency codes"
      :samples ["ACU" "BGX" "HSM" "NSB"])
    (sv-cases/false-case "currency symbols"
      :samples ["$" "₫" "€" "¤" "₹" "¥" "£" "₪" "₱" "₩"])
    (sv-cases/false-case "a non-string"
      :samples [true false 35.4 #{"GBP" "USD"}])
    (sv-cases/false-case "nil" :sample nil)))
