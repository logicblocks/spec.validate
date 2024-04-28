(ns spec.definition.logic.core-test
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as spec]
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]

   [spec.validate.core :as sv-core]

   [spec.definition.logic.core :as sd-logic]

   [datatype.testing.cases :as dt-test-cases]))

(deftest boolean?-as-predicate
  (doseq
   [case
    [(dt-test-cases/true-case "booleans"
       :samples [true false
                 Boolean/TRUE Boolean/FALSE
                 (Boolean/valueOf true) (Boolean/valueOf false)])
     (dt-test-cases/false-case "boolean strings"
       :samples ["true" "false" "TRUE" "FALSE" "True" "False"])
     (dt-test-cases/false-case "non-booleans"
       :samples ["the quick brown fox jumped over the lazy dog"
                 35.4 #{"GBP" "USD"}])
     (dt-test-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sd-logic/boolean?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest boolean?-as-requirement
  (is (= :must-be-a-boolean
        (sv-core/pred-requirement
          'spec.definition.logic.core/boolean?)))
  (is (= :must-be-a-boolean
        (sv-core/pred-requirement
          'clojure.core/boolean?))))

(deftest boolean?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample
              (spec/gen sd-logic/boolean?) 100)))))
  (testing "generates only true and false values"
    (let [booleans
          (into #{}
            (gen/sample
              (spec/gen sd-logic/boolean?)
              100))]
      (is (= #{true false} booleans)))))

(deftest boolean-string?-as-predicate
  (doseq
   [case
    [(dt-test-cases/true-case "boolean strings"
       :samples ["true" "false" "TRUE" "FALSE" "True" "False"])
     (dt-test-cases/false-case "booleans"
       :samples [true false
                 Boolean/TRUE Boolean/FALSE
                 (Boolean/valueOf true) (Boolean/valueOf false)])
     (dt-test-cases/false-case "non-strings"
       :samples [35.4 #{"GBP" "USD"}])
     (dt-test-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sd-logic/boolean-string?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest boolean-string?-as-requirement
  (is (= :must-be-a-boolean-string
        (sv-core/pred-requirement
          'spec.definition.logic.core/boolean-string?))))

(deftest boolean-string?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample
              (spec/gen sd-logic/boolean?) 100)))))
  (testing "generates various casings of true and false strings"
    (let [strings
          (into #{}
            (gen/sample
              (spec/gen sd-logic/boolean-string?)
              100))]
      (is (> (count strings) 2))
      (is (empty?
            (set/difference
              (into #{} (map string/lower-case strings))
              #{"true" "false"}))))))
