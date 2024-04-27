(ns datatype.address.gen-test
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.test :refer [deftest testing is]]

   [datatype.address.gen :as dt-address-gen]))

(deftest gen-uk-postcode-formatted-string
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample
              (dt-address-gen/gen-uk-postcode-formatted-string) 100)))))
  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample
              (dt-address-gen/gen-uk-postcode-formatted-string) 100)))))
  (testing "generates unique UK postcodes"
    (let [postcodes
          (into #{}
            (gen/sample
              (dt-address-gen/gen-uk-postcode-formatted-string)
              100))]
      (is (= (count postcodes) 100)))))
