(ns spec.definition.bool.core-test
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as spec]
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]

   [spec.validate.core :as sv-core]
   [spec.definition.bool.core]

   [datatype.testing.cases :as dt-test-cases]))

(deftest boolean-spec-predicate
  (dt-test-cases/assert-cases-satisfied-by
    (partial spec/valid? :datatype.bool/boolean)
    (dt-test-cases/true-case "booleans"
      :samples [true false
                Boolean/TRUE Boolean/FALSE
                (Boolean/valueOf true) (Boolean/valueOf false)])
    (dt-test-cases/false-case "boolean strings"
      :samples ["true" "false" "TRUE" "FALSE" "True" "False"])
    (dt-test-cases/false-case "non-booleans"
      :samples ["the quick brown fox jumped over the lazy dog"
                35.4 #{"GBP" "USD"}])
    (dt-test-cases/false-case "nil" :sample nil)))

(deftest boolean-spec-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample
              (spec/gen :datatype.bool/boolean) 100)))))
  (testing "generates only true and false values"
    (let [booleans
          (into #{}
            (gen/sample
              (spec/gen :datatype.bool/boolean)
              100))]
      (is (= #{true false} booleans)))))

(deftest boolean-pred-requirement
  (is (= :must-be-a-boolean
        (sv-core/pred-requirement
          'clojure.core/boolean?))))

(deftest boolean-string-spec-validation
  (dt-test-cases/assert-cases-satisfied-by
    (partial spec/valid? :datatype.bool/boolean-string)
    (dt-test-cases/true-case "boolean strings"
      :samples ["true" "false" "TRUE" "FALSE" "True" "False"])
    (dt-test-cases/false-case "booleans"
      :samples [true false
                Boolean/TRUE Boolean/FALSE
                (Boolean/valueOf true) (Boolean/valueOf false)])
    (dt-test-cases/false-case "non-strings"
      :samples [35.4 #{"GBP" "USD"}])
    (dt-test-cases/false-case "nil" :sample nil)))

(deftest boolean-string-spec-generation
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample
              (spec/gen :datatype.bool/boolean-string) 100)))))
  (testing "generates various casings of true and false strings"
    (let [strings
          (into #{}
            (gen/sample
              (spec/gen :datatype.bool/boolean-string)
              100))]
      (is (> (count strings) 2))
      (is (empty?
            (set/difference
              (into #{} (map string/lower-case strings))
              #{"true" "false"}))))))

(deftest boolean-string-pred-requirement
  (is (= :must-be-a-boolean-string
        (sv-core/pred-requirement
          'datatype.bool.core/boolean-string?))))
