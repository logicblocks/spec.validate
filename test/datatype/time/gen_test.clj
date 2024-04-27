(ns datatype.time.gen-test
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.test :refer [deftest testing is]]

   [cljc.java-time.offset-date-time :as jt-odt]

   [datatype.time.gen :as dt-time-gen]))

(deftest iso8601-zoned-datetime-string?-as-generator
  (testing "does not generate nil"
    (is
      (every? false?
        (map nil?
          (gen/sample (dt-time-gen/gen-iso8601-zoned-datetime-string) 100)))))
  (testing "does not generate empty strings"
    (is
      (every? false?
        (map #(= "" %)
          (gen/sample (dt-time-gen/gen-iso8601-zoned-datetime-string) 100)))))
  (testing "generates some past dates"
    (is (some?
          (map
            (fn [sample]
              (let [parsed (jt-odt/parse sample)
                    now (jt-odt/now)]
                (or
                  (jt-odt/is-before parsed now)
                  (jt-odt/is-equal parsed now))))
            (gen/sample (dt-time-gen/gen-iso8601-zoned-datetime-string) 100)))))
  (testing "generates some future dates"
    (is (some?
          (map
            (fn [sample]
              (let [parsed (jt-odt/parse sample)
                    now (jt-odt/now)]
                (or
                  (jt-odt/is-after parsed now)
                  (jt-odt/is-equal parsed now))))
            (gen/sample (dt-time-gen/gen-iso8601-zoned-datetime-string) 100)))))
  (testing "generates dates with various offsets"
    (let [offsets
          (into #{}
            (map
              (comp jt-odt/get-offset jt-odt/parse)
              (gen/sample (dt-time-gen/gen-iso8601-zoned-datetime-string) 100)))]
      (is (> (count offsets) 1)))))
