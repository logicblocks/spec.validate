(ns spec.validate.money-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.spec.alpha :as spec]
   [clojure.spec.gen.alpha :as gen]

   [spec.validate.core :as sv-core]
   [spec.validate.money :as sv-money]

   [spec.validate.test-support.cases :as sv-cases]))

(deftest iso4217-currency-code-string?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any currency code string"
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
     (sv-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sv-money/iso4217-currency-code-string?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest iso4217-currency-code-string?-as-requirement
  (is (= :must-be-an-iso4217-currency-code-string
        (sv-core/pred-requirement
          'spec.validate.money/iso4217-currency-code-string?))))

(deftest iso4217-currency-code-string?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample
              (spec/gen sv-money/iso4217-currency-code-string?) 100)))))
  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample
              (spec/gen sv-money/iso4217-currency-code-string?) 100)))))
  (testing "generates various currency codes"
    (let [codes
          (into #{}
            (gen/sample
              (spec/gen sv-money/iso4217-currency-code-string?)
              100))]
      (is (> (count codes) 20)))))