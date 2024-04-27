(ns datatype.email.gen-test
  (:require
   [clojure.set :as set]
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]
   [clojure.test :refer [deftest testing is]]

   [datatype.support :as dts]
   [datatype.email.gen :as dt-email-gen]
   [datatype.domain.core :as dt-domain]

   [icu4clj.text.unicode-set :as icu-tus]))

(deftest gen-email-address
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (dt-email-gen/gen-email-address) 100)))))
  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample (dt-email-gen/gen-email-address) 100)))))
  (testing "generates unique email addresses"
    (let [email-addresses
          (into #{}
            (gen/sample (dt-email-gen/gen-email-address) 100))]
      (is (= (count email-addresses) 100))))
  (testing "generates email addresses with real top level domain names"
    (let [email-addresses
          (gen/sample (dt-email-gen/gen-email-address) 100)
          top-level-domain-labels
          (into #{}
            (map
              (fn [email-address]
                (last
                  (string/split
                    (second
                      (string/split email-address #"@"))
                    #"\.")))
              email-addresses))]
      (is (empty?
            (set/difference
              top-level-domain-labels
              dt-domain/top-level-domain-labels)))))
  (testing "generates email addresses with valid second level domain names"
    (let [email-addresses
          (gen/sample
            (dt-email-gen/gen-email-address)
            100)
          second-level-domain-labels
          (into #{}
            (map
              (fn [email-address]
                (first (string/split (second (string/split email-address #"@")) #"\.")))
              email-addresses))]
      (is (every?
            (fn [sld]
              (dts/re-satisfies?
                #"^[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-zA-Z0-9])?$"
                sld))
            second-level-domain-labels))))
  (testing "generates email addresses with a variety of local parts"
    (let [email-addresses
          (gen/sample (dt-email-gen/gen-email-address) 100)
          local-parts
          (into #{}
            (map
              (fn [email-address]
                (first (string/split email-address #"@")))
              email-addresses))]
      (is (> (count local-parts) 50))))
  (testing "generates email addresses with different length local parts"
    (let [email-addresses
          (gen/sample (dt-email-gen/gen-email-address) 100)
          local-parts
          (into #{}
            (map
              (fn [email-address]
                (first (string/split email-address #"@")))
              email-addresses))
          local-part-lengths
          (into #{} (map count local-parts))]
      (is (> (count local-part-lengths) 20))))
  (testing "generates email addresses with correct character set for local part"
    (let [email-addresses
          (gen/sample (dt-email-gen/gen-email-address))
          local-parts
          (map
            (fn [email-address]
              (first (string/split email-address #"@")))
            email-addresses)
          local-part-characters
          (reduce
            (fn [acc local-part]
              (into acc (map str local-part)))
            #{}
            local-parts)]
      (is (empty?
            (set/difference
              local-part-characters
              (icu-tus/character-set
                "[a-zA-Z0-9\\^\\&.!#$%'*+/\\=?_`{|}~-]")))))))
