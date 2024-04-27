(ns datatype.bool.gen-test
  (:require
   [clojure.set :as set]
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]
   [clojure.test :refer [deftest testing is]]

   [datatype.bool.gen :as dt-bool-gen]))

(deftest gen-boolean-string
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (dt-bool-gen/gen-boolean-string) 100)))))
  (testing "generates various casings of true and false strings"
    (let [strings
          (into #{}
            (gen/sample (dt-bool-gen/gen-boolean-string) 100))]
      (is (> (count strings) 2))
      (is (empty?
            (set/difference
              (into #{} (map string/lower-case strings))
              #{"true" "false"}))))))
