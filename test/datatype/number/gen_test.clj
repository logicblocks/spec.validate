(ns datatype.number.gen-test
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.test :refer [deftest testing is]]

   [datatype.support :as dts]
   [datatype.number.gen :as dt-number-gen]))

(deftest gen-integer-string
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (dt-number-gen/gen-integer-string) 100)))))

  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample (dt-number-gen/gen-integer-string) 100)))))

  (testing "generates integer strings"
    (let [samples (gen/sample (dt-number-gen/gen-integer-string))
          pred #(or (= "0" %) (dts/re-satisfies? #"^[-+]?[1-9]\d*$" %))]
      (is (every? true? (map pred samples))
        (str "mismatching samples: "
          (filterv #(not (pred %)) samples))))))

(deftest decimal-string?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (dt-number-gen/gen-decimal-string) 100)))))

  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample (dt-number-gen/gen-decimal-string) 100)))))

  (testing "generates decimal strings"
    (let [samples (gen/sample (dt-number-gen/gen-decimal-string))
          can-parse?
          (fn [x]
            (try
              (boolean (parse-double x))
              (catch Exception _ false)))]
      (is (every? true? (map can-parse? samples))
        (str "mismatching samples: "
          (filterv #(not (can-parse? %)) samples))))))
