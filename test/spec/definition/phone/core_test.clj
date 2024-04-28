(ns spec.definition.phone.core-test
  (:require
   [clojure.spec.alpha :as spec]
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]

   [spec.validate.core :as sv-core]

   [spec.definition.phone.core :as sd-phone]

   [datatype.testing.cases :as dt-test-cases])
  (:import
   [com.google.i18n.phonenumbers
    PhoneNumberUtil
    Phonenumber$PhoneNumber]))

(deftest phone-number-string?-as-predicate
  (doseq
   [case
    [(dt-test-cases/true-case "any non-international GB phone number"
       :samples ["02078910111"
                 "07891400100"
                 "08001002000"
                 "08451234567"
                 "0207 891 0111"
                 "07891 400 100"
                 "0800 100 2000"
                 "0845 123 4567"])
     (dt-test-cases/true-case "any international phone number"
       :samples ["+12126885500"
                 "+1-212-688-5500"
                 "+5551989063736"
                 "+55 (51) 98906 3736"
                 "+55-(51)-98906-3736"])
     (dt-test-cases/false-case "any non-international non-GB phone number"
       :samples ["2126885500"
                 "212-688-5500"
                 "51989063736"
                 "(51) 98906 3736"
                 "(51)-98906-3736"])
     (dt-test-cases/false-case "strings that aren't UUID-like at all"
       :samples ["the quick brown fox jumped over the lazy dog"
                 "23.6"
                 "true"])
     (dt-test-cases/false-case "a non-string"
       :samples [true false 35.4 #{"GBP" "USD"}])
     (dt-test-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sd-phone/phone-number-string?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest phone-number-string?-as-requirement
  (is (= :must-be-a-phone-number-string
        (sv-core/pred-requirement
          'spec.definition.phone.core/phone-number-string?))))

(defn parse-phone-number ^Phonenumber$PhoneNumber [phone-number-string]
  (.parse (PhoneNumberUtil/getInstance) phone-number-string "GB"))

(defn country-code [phone-number-string]
  (.getCountryCode (parse-phone-number phone-number-string)))

(deftest phone-number-string?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample
              (spec/gen sd-phone/phone-number-string?) 100)))))
  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample
              (spec/gen sd-phone/phone-number-string?) 100)))))
  (testing "generates unique phone number strings"
    (let [phone-number-strings
          (into #{}
            (gen/sample
              (spec/gen sd-phone/phone-number-string?)
              100))]
      (is (= (count phone-number-strings) 100))))
  (testing
   (str "generates some international phone numbers with a variety of "
     "country codes")
    (let [phone-number-strings
          (into #{}
            (gen/sample
              (spec/gen sd-phone/phone-number-string?)
              100))
          country-codes
          (map country-code phone-number-strings)]
      (is (> (count country-codes) 20))))
  (testing "generates some national phone numbers"
    (let [phone-number-strings
          (into #{}
            (gen/sample
              (spec/gen sd-phone/phone-number-string?)
              100))
          national-phone-numbers
          (filter
            (fn [pn] (not (string/starts-with? pn "+")))
            phone-number-strings)]
      (is (> (count national-phone-numbers) 20)))))
