(ns spec.definition.uuid.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.spec.alpha :as spec]
   [clojure.spec.gen.alpha :as gen]

   [spec.validate.core :as sv-core]
   [spec.definition.uuid.core]

   [datatype.testing.cases :as dt-test-cases]))

(deftest uuid-string-spec-validation
  (dt-test-cases/assert-cases-satisfied-by
    (partial spec/valid? :datatype.uuid/uuid-string)
    (dt-test-cases/true-case "any UUID string"
      :samples ["c09ac12c-036a-43b4-9ac1-2c036a43b4c9"
                "0081F399-287F-42CE-81F3-99287F22CE3D"
                "00000000-0000-0000-0000-000000000000"
                "ffffffff-ffff-ffff-ffff-ffffffffffff"
                "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"])
    (dt-test-cases/false-case "incorrectly formatted UUID strings"
      :samples ["c09a-c12c-036a-43b4-9ac1-2c03-6a43-b4c9"
                "0081F399287F42CE81F399287F22CE3D"
                "00000000-0000-0000-0000-0000"])
    (dt-test-cases/false-case "strings that aren't UUID-like at all"
      :samples ["the quick brown fox jumped over the lazy dog"
                "23.6"
                "true"])
    (dt-test-cases/false-case "a non-string"
      :samples [true false 35.4 #{"GBP" "USD"}])
    (dt-test-cases/false-case "nil" :sample nil)))

(deftest uuid-string-spec-generation
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample
              (spec/gen :datatype.uuid/uuid-string) 100)))))
  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample
              (spec/gen :datatype.uuid/uuid-string) 100)))))
  (testing "generates unique UUIDs"
    (let [codes
          (into #{}
            (gen/sample
              (spec/gen :datatype.uuid/uuid-string)
              100))]
      (is (= (count codes) 100)))))

(deftest uuid-string-pred-requirement
  (is (= :must-be-a-uuid-string
        (sv-core/pred-requirement
          'datatype.uuid.core/uuid-string?))))
