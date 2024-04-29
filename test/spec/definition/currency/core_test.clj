(ns spec.definition.currency.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.spec.alpha :as spec]
   [clojure.spec.gen.alpha :as gen]

   [spec.validate.core :as sv-core]
   [spec.definition.currency.core]

   [datatype.testing.cases :as dt-test-cases]))

(deftest iso4217-currency-code-string-spec-validation
  (dt-test-cases/assert-cases-satisfied-by
    (partial spec/valid? :datatype.currency/iso4217-currency-code-string)
    (dt-test-cases/true-case "any currency code string"
      :samples ["GBP" "BAM" "CUC" "MKN" "BOV" "ARP"])
    (dt-test-cases/false-case "valid but lowercase currency code strings"
      :samples ["gbp" "bam" "cuc" "mkn" "bov" "arp"])
    (dt-test-cases/false-case "additional whitespace"
      :samples [" GBP " "\tBAM" " CUC "])
    (dt-test-cases/false-case "any monetary amount string"
      :samples ["GBP 500.25" "BAM 0.15" "CUC 1000" "MKN -30.28"])
    (dt-test-cases/false-case
      "3 letter uppercase strings that look like currency codes"
      :samples ["ACU" "BGX" "HSM" "NSB"])
    (dt-test-cases/false-case "currency symbols"
      :samples ["$" "₫" "€" "¤" "₹" "¥" "£" "₪" "₱" "₩"])
    (dt-test-cases/false-case "a non-string"
      :samples [true false 35.4 #{"GBP" "USD"}])
    (dt-test-cases/false-case "nil" :sample nil)))

(deftest iso4217-currency-code-string-spec-generation
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample
              (spec/gen :datatype.currency/iso4217-currency-code-string)
              100)))))
  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample
              (spec/gen :datatype.currency/iso4217-currency-code-string)
              100)))))
  (testing "generates various currency codes"
    (let [codes
          (into #{}
            (gen/sample
              (spec/gen :datatype.currency/iso4217-currency-code-string)
              100))]
      (is (> (count codes) 20)))))

(deftest iso4217-currency-code-string-pred-requirement
  (is (= :must-be-an-iso4217-currency-code-string
        (sv-core/pred-requirement
          'datatype.currency.core/iso4217-currency-code-string?))))
