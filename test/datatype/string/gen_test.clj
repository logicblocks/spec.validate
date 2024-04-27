(ns datatype.string.gen-test
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.test :refer [deftest testing is]]

   [datatype.support :as dts]
   [datatype.string.gen :as dt-string-gen]

   [icu4clj.text.unicode-characters :as icu-tuc]
   [icu4clj.text.string :as icu-ts]))

(deftest gen-string-whitespace
  (testing "generates some empty strings"
    (let [samples (gen/sample (dt-string-gen/gen-string-whitespace) 100)]
      (is (not (empty?
                 (filter
                   (fn [sample] (= 0 (count sample)))
                   samples))))))

  (testing "generates only whitespace strings"
    (let [samples (gen/sample (dt-string-gen/gen-string-whitespace) 100)]
      (is (every? true?
            (map
              (fn [sample]
                (every?
                  icu-tuc/whitespace-character?
                  (icu-ts/codepoint-seq sample)))
              samples))))))

(deftest gen-string-non-whitespace
  (testing "generates no empty strings when requested"
    (let [samples (gen/sample
                    (dt-string-gen/gen-string-non-whitespace
                      {:allow-empty? false})
                    100)]
      (is (empty?
            (filter
              (fn [sample] (= 0 (count sample)))
              samples)))))

  (testing "generates only non-whitespace strings"
    (let [samples (gen/sample
                    (dt-string-gen/gen-string-non-whitespace
                      {:allow-empty? false})
                    100)]
      (is (every? true?
            (map
              (fn [sample]
                (some
                  icu-tuc/non-whitespace-character?
                  (icu-ts/codepoint-seq sample)))
              samples))))))

(deftest gen-string-ascii-digits
  (testing "generates no empty strings when requested"
    (is (empty?
          (filter
            (fn [sample] (= 0 (count sample)))
            (gen/sample
              (dt-string-gen/gen-string-ascii-digits
                {:allow-empty? false})
              100)))))

  (testing "generates only ASCII digit strings"
    (is (every? true?
          (map #(dts/re-satisfies? #"^\d+$" %)
            (gen/sample
              (dt-string-gen/gen-string-ascii-digits
                {:allow-empty? false})
              100))))))

(deftest gen-string-lowercase-ascii-alphabetics
  (testing "generates no empty strings when requested"
    (is (empty?
          (filter
            (fn [sample] (= 0 (count sample)))
            (gen/sample
              (dt-string-gen/gen-string-lowercase-ascii-alphabetics
                {:allow-empty? false})
              100)))))

  (testing "generates only lowercase ASCII alphabetic strings"
    (is (every? true?
          (map #(dts/re-satisfies? #"^[a-z]+$" %)
            (gen/sample
              (dt-string-gen/gen-string-lowercase-ascii-alphabetics
                {:allow-empty? false})
              100))))))

(deftest gen-string-uppercase-ascii-alphabetics
  (testing "generates no empty strings when requested"
    (is (empty?
          (filter
            (fn [sample] (= 0 (count sample)))
            (gen/sample
              (dt-string-gen/gen-string-uppercase-ascii-alphabetics
                {:allow-empty? false})
              100)))))

  (testing "generates only uppercase ASCII alphabetic strings"
    (is (every? true?
          (map #(dts/re-satisfies? #"^[A-Z]+$" %)
            (gen/sample
              (dt-string-gen/gen-string-uppercase-ascii-alphabetics
                {:allow-empty? false})
              100))))))

(deftest gen-string-ascii-alphabetics
  (testing "generates no empty strings when requested"
    (is (empty?
          (filter
            (fn [sample] (= 0 (count sample)))
            (gen/sample
              (dt-string-gen/gen-string-ascii-alphabetics
                {:allow-empty? false})
              100)))))

  (testing "generates only ASCII alphabetic strings"
    (is (every? true?
          (map #(dts/re-satisfies? #"^[a-zA-Z]+$" %)
            (gen/sample
              (dt-string-gen/gen-string-ascii-alphabetics
                {:allow-empty? false})
              100))))))

(deftest gen-string-lowercase-ascii-alphanumerics
  (testing "generates no empty strings when requested"
    (is (empty?
          (filter
            (fn [sample] (= 0 (count sample)))
            (gen/sample
              (dt-string-gen/gen-string-lowercase-ascii-alphanumerics
                {:allow-empty? false})
              100)))))

  (testing "generates only lowercase ASCII alphanumeric strings"
    (is (every? true?
          (map #(dts/re-satisfies? #"^[a-z0-9]+$" %)
            (gen/sample
              (dt-string-gen/gen-string-lowercase-ascii-alphanumerics
                {:allow-empty? false})
              100))))))

(deftest gen-string-uppercase-ascii-alphanumerics
  (testing "generates no empty strings when requested"
    (is (empty?
          (filter
            (fn [sample] (= 0 (count sample)))
            (gen/sample
              (dt-string-gen/gen-string-uppercase-ascii-alphanumerics
                {:allow-empty? false})
              100)))))

  (testing "generates only uppercase ASCII alphanumeric strings"
    (is (every? true?
          (map #(dts/re-satisfies? #"^[A-Z0-9]+$" %)
            (gen/sample
              (dt-string-gen/gen-string-uppercase-ascii-alphanumerics
                {:allow-empty? false})
              100))))))

(deftest gen-string-ascii-alphanumerics
  (testing "generates no empty strings when requested"
    (is (empty?
          (filter
            (fn [sample] (= 0 (count sample)))
            (gen/sample
              (dt-string-gen/gen-string-ascii-alphanumerics
                {:allow-empty? false})
              100)))))

  (testing "generates only ASCII alphabetic strings"
    (is (every? true?
          (map #(dts/re-satisfies? #"^[a-zA-Z0-9]+$" %)
            (gen/sample
              (dt-string-gen/gen-string-ascii-alphanumerics
                {:allow-empty? false})
              100))))))
