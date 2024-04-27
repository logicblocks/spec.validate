(ns datatype.phone.gen-test
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]
   [clojure.test :refer [deftest testing is]]

   [datatype.phone.gen :as dt-phone-gen])
  (:import
   [com.google.i18n.phonenumbers
    PhoneNumberUtil
    Phonenumber$PhoneNumber]))

(defn parse-phone-number ^Phonenumber$PhoneNumber [phone-number-string]
  (.parse (PhoneNumberUtil/getInstance) phone-number-string "GB"))

(defn country-code [phone-number-string]
  (.getCountryCode (parse-phone-number phone-number-string)))

(deftest phone-number-string?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (dt-phone-gen/gen-phone-number) 100)))))
  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample (dt-phone-gen/gen-phone-number) 100)))))
  (testing "generates unique phone number strings"
    (let [phone-number-strings
          (into #{}
            (gen/sample (dt-phone-gen/gen-phone-number) 100))]
      (is (= (count phone-number-strings) 100))))
  (testing
   (str "generates some international phone numbers with a variety of "
     "country codes")
    (let [phone-number-strings
          (into #{}
            (gen/sample (dt-phone-gen/gen-phone-number) 100))
          country-codes
          (map country-code phone-number-strings)]
      (is (> (count country-codes) 20))))
  (testing "generates some national phone numbers"
    (let [phone-number-strings
          (into #{}
            (gen/sample (dt-phone-gen/gen-phone-number) 100))
          national-phone-numbers
          (filter
            (fn [pn] (not (string/starts-with? pn "+")))
            phone-number-strings)]
      (is (> (count national-phone-numbers) 20)))))
