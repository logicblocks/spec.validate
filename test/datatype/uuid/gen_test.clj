(ns datatype.uuid.gen-test
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.test :refer [deftest testing is]]

   [datatype.uuid.gen :as dt-uuid-gen]))

(deftest gen-uuid-string
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (dt-uuid-gen/gen-uuid-string) 100)))))
  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample (dt-uuid-gen/gen-uuid-string) 100)))))
  (testing "generates unique UUIDs"
    (let [codes
          (into #{}
            (gen/sample (dt-uuid-gen/gen-uuid-string) 100))]
      (is (= (count codes) 100)))))
