(ns datatype.currency.gen-test
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.test :refer [deftest testing is]]

   [datatype.currency.gen :as dt-currency-gen]))

(deftest iso4217-currency-code-string?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (dt-currency-gen/gen-currency-code) 100)))))
  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample (dt-currency-gen/gen-currency-code) 100)))))
  (testing "generates various currency codes"
    (let [codes
          (into #{}
            (gen/sample (dt-currency-gen/gen-currency-code) 100))]
      (is (> (count codes) 20)))))
