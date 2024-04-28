(ns spec.definition.address.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.spec.alpha :as spec]
   [clojure.spec.gen.alpha :as gen]

   [spec.validate.core :as sv-core]

   [spec.definition.address.core :as sd-address]

   [datatype.testing.cases :as dt-test-cases]))

(deftest uk-postcode-formatted-string?-as-predicate
  (doseq
   [case
    [(dt-test-cases/true-case "any uppercase standard format UK postcode"
       :samples ["EC1A 1BB"
                 "W1A 0AX"
                 "M1 1AE"
                 "B33 8TH"
                 "CR2 6XH"
                 "DN55 1PT"])
     (dt-test-cases/true-case "any lowercase standard format UK postcode"
       :samples ["ec1a 1bb"
                 "w1a 0ax"
                 "m1 1ae"
                 "b33 8th"
                 "cr2 6xh"
                 "dn55 1pt"])
      ; TODO: should this be false since the Crown Dependencies aren't the UK?
     (dt-test-cases/true-case "any Crown Dependency postcode"
       :samples ["GY8 0HG" ; Guernsey
                 "IM1 5PD" ; Isle of Man
                 "JE2 3AB" ; Jersey
                 ])
      ; TODO: should this be false since gibraltar is a British Overseas
      ; Territory?
     (dt-test-cases/true-case "Gibraltar"
       :sample "GX11 1AA" ; Gibraltar
       )
      ; TODO: should this be true or false?
     (dt-test-cases/true-case "British Forces Post Office post codes"
       :samples ["BF1 1AA"
                 "BF1 2AN"])
     (dt-test-cases/false-case "some British Overseas Territories"
       :samples ["ASCN 1ZZ" ; Ascension island
                 "BBND 1ZZ" ; British Indian Ocean Territory
                 "BIQQ 1ZZ" ; British Antarctic Territory
                 "FIQQ 1ZZ" ; Falkland Islands
                 "PCRN 1ZZ" ; Pitcairn Islands
                 "SIQQ 1ZZ" ; South Georgia and the South Sandwich Islands
                 "STHL 1ZZ" ; Saint Helena
                 "TDCU 1ZZ" ; Tristan da Cunha
                 "TKCA 1ZZ" ; Turks and Caicos Islands
                 "AI-2640"  ; Anguilla
                 "KY1-1600" ; Cayman Islands
                 "MSR-1110" ; Montserrat
                 "VG-1130"  ; British Virgin Islands
                 "WK 06"    ; Bermuda
                 ])
     (dt-test-cases/false-case "British Forces Post Office (BFPO) codes"
       :samples ["BFPO 801"])
     (dt-test-cases/false-case "strings that aren't postcode-like at all"
       :samples ["the quick brown fox jumped over the lazy dog"
                 "23.6"
                 "true"])
     (dt-test-cases/false-case "a non-string"
       :samples [true false 35.4 #{"GBP" "USD"}])
     (dt-test-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sd-address/uk-postcode-formatted-string?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest uk-postcode-formatted-string?-as-requirement
  (is (= :must-be-a-uk-postcode
        (sv-core/pred-requirement
          'spec.definition.address.core/uk-postcode-formatted-string?))))

(deftest uk-postcode-formatted-string?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample
              (spec/gen sd-address/uk-postcode-formatted-string?) 100)))))
  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample
              (spec/gen sd-address/uk-postcode-formatted-string?) 100)))))
  (testing "generates unique UK postcodes"
    (let [postcodes
          (into #{}
            (gen/sample
              (spec/gen sd-address/uk-postcode-formatted-string?)
              100))]
      (is (= (count postcodes) 100)))))
