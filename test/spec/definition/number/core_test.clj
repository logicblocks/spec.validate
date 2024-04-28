(ns spec.definition.number.core-test
  (:require
   [clojure.spec.alpha :as spec]
   [clojure.spec.gen.alpha :as gen]
   [clojure.test :refer [deftest is testing]]

   [spec.validate.core :as sv-core]

   [spec.definition.core :as sd]
   [spec.definition.number.core :as sd-number]

   [datatype.testing.cases :as dt-test-cases])
  (:import [java.util Locale]))

(deftest integer?-as-predicate
  (doseq
   [case
    [(dt-test-cases/true-case "any integer"
       :samples [0 -0 +0 35 -35 +35
                 0xff 017 2r1011 7N 36rCRAZY
                 Integer/MIN_VALUE
                 Integer/MAX_VALUE])
     (dt-test-cases/false-case "a decimal" :sample 35.23)
     (dt-test-cases/false-case "a non-integer"
       :samples [true false "not-an-integer"])
     (dt-test-cases/false-case "an integer string" :sample "12345")
     (dt-test-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sd-number/integer?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest integer?-as-requirement
  (is (= :must-be-an-integer
        (sv-core/pred-requirement
          'spec.definition.number.core/integer?))))

(deftest clojure-core-integer?-as-requirement
  (is (= :must-be-an-integer
        (sv-core/pred-requirement
          'clojure.core/integer?))))

(deftest integer?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (spec/gen sd-number/integer?) 100)))))
  (testing "generates integers"
    (is (every? true?
          (map #(clojure.core/integer? %)
            (gen/sample (spec/gen sd-number/integer?)))))))

(deftest integer-string?-as-predicate
  (testing "general cases"
    (doseq
     [case
      [(dt-test-cases/true-case "any integer as a string"
         :samples ["0" "-0" "+0" "35" "-35" "+35"
                   (str Integer/MIN_VALUE)
                   (str Integer/MAX_VALUE)])
       (dt-test-cases/false-case "any non-base 10 integer as a string"
         :samples ["0xff" "035" "2r1011" "7N" "36rCRAZY"])
       (dt-test-cases/false-case "many zeroes"
         :samples ["00", "0000000"])
       (dt-test-cases/false-case "leading zeroes"
         :samples ["00123" "-00123"])
       (dt-test-cases/false-case "multiple leading signs"
         :samples ["+-123" "-+45" "--0" "++10" "+-+23"])
       (dt-test-cases/false-case "any integer"
         :samples [0 35 -35
                   0xff 017 2r1011 7N 36rCRAZY
                   Integer/MIN_VALUE
                   Integer/MAX_VALUE])
       (dt-test-cases/false-case "a decimal" :sample 35.23)
       (dt-test-cases/false-case "a non-numeric string"
         :sample "not-an-integer")
       (dt-test-cases/false-case "a non-string"
         :samples [true false #{1 2 3}])
       (dt-test-cases/false-case "an empty string"
         :sample "")
       (dt-test-cases/false-case "nil" :sample nil)]]
      (let [{:keys [samples satisfied? title locale]} case
            pred sd-number/integer-string?]
        (testing (str "for " title)
          (dt-test-cases/with-locale locale
            (is (every? #(= % satisfied?) (map pred samples))
              (str "unsatisfied for: "
                (into #{}
                  (filter #(not (= (pred %) satisfied?)) samples)))))))))

  (testing "locale specific cases"
    (doseq
     [case
      [(dt-test-cases/true-case
         "an integer string with correct thousands separator for the UK"
         :samples ["1,000" "10,000" "100,000" "1,000,000"]
         :locale (Locale/UK))
       (dt-test-cases/false-case
         "an integer string with incorrect thousands separator for the UK"
         :samples ["1.000" "10.000" "100.000" "1.000.000"]
         :locale (Locale/UK))
       (dt-test-cases/true-case
         "an integer string with correct thousands separator for Germany"
         :samples ["1.000" "10.000" "100.000" "1.000.000"]
         :locale (Locale/GERMANY))
       (dt-test-cases/false-case
         "an integer string with incorrect thousands separator for Germany"
         :samples ["1,000" "10,000" "100,000" "1,000,000"]
         :locale (Locale/GERMANY))
       (dt-test-cases/true-case
         "an integer string with thousands separator in the wrong place"
         :samples ["1,0" "1,00,0000"]
         :locale (Locale/UK)
         :note "TODO: This is treated as valid but shouldn't be...")
       (dt-test-cases/false-case "a decimal string"
         :sample "35.23"
         :locale (Locale/UK))]]
      (let [{:keys [samples satisfied? title locale]} case
            pred sd-number/integer-string?]
        (testing (str "for " title)
          (dt-test-cases/with-locale locale
            (is (every? #(= % satisfied?) (map pred samples))
              (str "unsatisfied for: "
                (into #{} (filter #(not (= (pred %) satisfied?))
                            samples))))))))))

(deftest integer-string?-as-requirement
  (is (= :must-be-an-integer-string
        (sv-core/pred-requirement
          'spec.definition.number.core/integer-string?))))

(deftest integer-string?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (spec/gen sd-number/integer-string?) 100)))))

  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample (spec/gen sd-number/integer-string?) 100)))))

  (testing "generates integer strings"
    (let [samples (gen/sample (spec/gen sd-number/integer-string?))
          pred #(or (= "0" %) (sd/re-satisfies? #"^[-+]?[1-9]\d*$" %))]
      (is (every? true? (map pred samples))
        (str "mismatching samples: "
          (filterv #(not (pred %)) samples))))))

(deftest floating-point-number?-as-predicate
  (doseq
   [case
    [(dt-test-cases/true-case "any floating point literal"
       :samples [0. -0. +0. 35. -35. +35. 2.78 0.82 -1.2e5 +1.2e5])
     (dt-test-cases/true-case "any boxed floating point number"
       :samples [(Double/parseDouble "1.23") (Float/parseFloat "3.45")])
     (dt-test-cases/true-case "any special case floating point value"
       :samples [Float/MIN_VALUE
                 Float/MAX_VALUE
                 Float/POSITIVE_INFINITY
                 Float/NEGATIVE_INFINITY
                 Float/NaN
                 Double/MAX_VALUE
                 Double/MIN_VALUE
                 Double/POSITIVE_INFINITY
                 Double/NEGATIVE_INFINITY
                 Double/NaN])
     (dt-test-cases/false-case "an integer literal"
       :samples [0 35])
     (dt-test-cases/false-case "a non-number"
       :samples [true false "not-a-floating-point-number"])
     (dt-test-cases/false-case "an decimal string" :sample "12345")
     (dt-test-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sd-number/floating-point-number?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest floating-point-number?-as-requirement
  (is (= :must-be-a-floating-point-number
        (sv-core/pred-requirement
          'spec.definition.number.core/floating-point-number?))))

(deftest floating-point-number?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (spec/gen sd-number/floating-point-number?) 100)))))

  (testing "generates floating point numbers"
    (let [samples (gen/sample (spec/gen sd-number/floating-point-number?))
          pred float?]
      (is (every? true? (map pred samples))
        (str "mismatching samples: "
          (into #{} (filterv #(not (pred %)) samples)))))))

(deftest decimal-string?-as-predicate
  (testing "general cases"
    (doseq
     [case
      [(dt-test-cases/true-case "any decimal as a string"
         :samples ["0" "-0" "+0" "0." "-0." "+0."
                   "0.00" "-0.0000" "+0.000"
                   "0.82" "+0.82" "-0.82"
                   "35." "-35." "+35." "35" "-35" "+35"
                   "2.78" ".91" "+.72"])
       (dt-test-cases/false-case "any non-base 10 integer as a string"
         :samples ["0xff" "035" "2r1011" "7N" "36rCRAZY"])
       (dt-test-cases/false-case "many zeroes"
         :samples ["00", "0000000"])
       (dt-test-cases/false-case "leading zeroes"
         :samples ["00123" "00.123" "000.456"])
       (dt-test-cases/false-case "multiple leading signs"
         :samples ["+-123.4" "-+45" "--0.23" "++10.0" "+-+23.456"])
       (dt-test-cases/false-case "any integer"
         :samples [0 35 -35
                   0xff 017 2r1011 7N 36rCRAZY
                   Integer/MIN_VALUE
                   Integer/MAX_VALUE])
       (dt-test-cases/false-case "a floating point number" :sample 35.23)
       (dt-test-cases/false-case "a non-numeric string"
         :sample "not-a-decimal")
       (dt-test-cases/false-case "a non-string"
         :samples [true false #{1 2 3}])
       (dt-test-cases/false-case "an empty string"
         :sample "")
       (dt-test-cases/false-case "nil" :sample nil)]]
      (let [{:keys [samples satisfied? title locale]} case
            pred sd-number/decimal-string?]
        (testing (str "for " title)
          (dt-test-cases/with-locale locale
            (is (every? #(= % satisfied?) (map pred samples))
              (str "unsatisfied for: "
                (into #{}
                  (filter #(not (= (pred %) satisfied?)) samples)))))))))

  (testing "locale specific cases"
    (doseq
     [case
      [(dt-test-cases/true-case
         "a decimal string with correct thousands separator for the UK"
         :samples ["1,000.456" "10,000" "100,000.1" "1,000,000."]
         :locale (Locale/UK))
       (dt-test-cases/false-case
         "a decimal string with incorrect thousands separator for the UK"
         :samples ["1.000,56" "10.000," "100.000,1" "1.000.000,"]
         :locale (Locale/UK))
       (dt-test-cases/true-case
         "a decimal string with correct thousands separator for Germany"
         :samples ["1.000,123" "10.000" "100.000,4" "1.000.000,"]
         :locale (Locale/GERMANY))
       (dt-test-cases/false-case
         "a decimal string with incorrect thousands separator for Germany"
         :samples ["1,000.456" "10,000.0" "100,000.1" "1,000,000."]
         :locale (Locale/GERMANY))
       (dt-test-cases/true-case
         "a decimal string with correct thousands separator for France"
         :samples ["1 000,456"
                   "10 000,0"
                   "100 000,1"
                   "1 000 000,"]
         :locale (Locale/FRANCE))
       (dt-test-cases/false-case
         "a decimal string with incorrect thousands separator for France"
         :samples ["1,000.123" "10.000,0" "100,000.4" "1.000.000,"]
         :locale (Locale/FRANCE))
       (dt-test-cases/true-case
         "a decimal string with thousands separator in the wrong place"
         :samples ["1,0.1" "1,00,0000."]
         :locale (Locale/UK)
         :note "TODO: This is treated as valid but shouldn't be...")]]
      (let [{:keys [samples satisfied? title locale]} case
            pred sd-number/decimal-string?]
        (testing (str "for " title)
          (dt-test-cases/with-locale locale
            (is (every? #(= % satisfied?) (map pred samples))
              (str "unsatisfied for: "
                (into #{} (filter #(not (= (pred %) satisfied?))
                            samples))))))))))

(deftest decimal-string?-as-requirement
  (is (= :must-be-a-decimal-string
        (sv-core/pred-requirement
          'spec.definition.number.core/decimal-string?))))

(deftest decimal-string?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (spec/gen sd-number/decimal-string?) 100)))))

  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample (spec/gen sd-number/decimal-string?) 100)))))

  (testing "generates decimal strings"
    (let [samples (gen/sample (spec/gen sd-number/decimal-string?))
          can-parse?
          (fn [x]
            (try
              (boolean (parse-double x))
              (catch Exception _ false)))]
      (is (every? true? (map can-parse? samples))
        (str "mismatching samples: "
          (filterv #(not (can-parse? %)) samples))))))

(deftest positive?-as-predicate
  (doseq
   [case
    [(dt-test-cases/true-case "any positive int"
       :samples [(int 1) (int 35) (int 100000) Integer/MAX_VALUE])
     (dt-test-cases/true-case "any positive long"
       :samples [1 35 100000 Long/MAX_VALUE])
     (dt-test-cases/true-case "any positive float"
       :samples [(float 0.00000000000001)
                 (float 1.23) (float 35.456) (float 100000)
                 Float/MIN_VALUE
                 Float/MAX_VALUE])
     (dt-test-cases/true-case "any positive double"
       :samples [0.000000000000000000001
                 1.23 35.456 100000
                 Double/MIN_VALUE
                 Double/MAX_VALUE])
     (dt-test-cases/true-case "any positive big int"
       :samples [1N 35N 100000N
                 (bigint "111111111111111999999999999991111111")])
     (dt-test-cases/true-case "any positive big integer"
       :samples [(biginteger 1) (biginteger 35) (biginteger 100000)
                 (biginteger "111111111111111999999999999991111111")])
     (dt-test-cases/true-case "any positive big decimal"
       :samples [(bigdec 1.123) (bigdec 35.2) (bigdec 100000)
                 (bigdec "111111111111111999999999999991111111.4567")])
     (dt-test-cases/false-case "any zero value number"
       :samples [(int 0) 0 (float 0) 0.0 0M 0N (biginteger 0)])
     (dt-test-cases/false-case "any negative integer"
       :samples [(int -1) (int -35) (int -100000) Integer/MIN_VALUE])
     (dt-test-cases/false-case "any negative long"
       :samples [-1 -35 -100000 Long/MIN_VALUE])
     (dt-test-cases/false-case "any negative float"
       :samples [(float -0.00000000000001)
                 (float -1.23) (float -35.456) (float -100000)
                 (- Float/MIN_VALUE)
                 (- Float/MAX_VALUE)])
     (dt-test-cases/false-case "any negative double"
       :samples [-0.000000000000000000001
                 -1.23 -35.456 -100000
                 (- Double/MIN_VALUE)
                 (- Double/MAX_VALUE)])
     (dt-test-cases/false-case "any negative big int"
       :samples [(biginteger -1) (biginteger -35) (biginteger -100000)
                 (bigint "-111111111111111999999999999991111111")])
     (dt-test-cases/false-case "any negative big integer"
       :samples [-1N -35N -100000N
                 (biginteger "-111111111111111999999999999991111111")])
     (dt-test-cases/false-case "any negative big decimal"
       :samples [(bigdec -1.123) (bigdec -35.2) (bigdec -100000)
                 (bigdec "-111111111111111999999999999991111111.4567")])
     (dt-test-cases/false-case "any number as a string"
       :samples ["1" "-1" "100.2" "-100.45"])
     (dt-test-cases/false-case "a non-number"
       :samples [true false "not-an-integer"])
     (dt-test-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sd-number/positive?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest positive?-as-requirement
  (is (= :must-be-a-positive-number
        (sv-core/pred-requirement
          'spec.definition.number.core/positive?))))

(deftest clojure-core-pos?-as-requirement
  (is (= :must-be-a-positive-number
        (sv-core/pred-requirement
          'clojure.core/pos?))))

(deftest clojure-core-pos-int?-as-requirement
  (is (= :must-be-a-positive-integer
        (sv-core/pred-requirement
          'clojure.core/pos-int?))))

(deftest positive?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (spec/gen sd-number/positive?) 100)))))
  (testing "generates positive numbers"
    (is (every? true?
          (map #(clojure.core/pos? %)
            (gen/sample (spec/gen sd-number/positive?)))))))

(deftest negative?-as-predicate
  (doseq
   [case
    [(dt-test-cases/true-case "any negative integer"
       :samples [(int -1) (int -35) (int -100000) Integer/MIN_VALUE])
     (dt-test-cases/true-case "any negative long"
       :samples [-1 -35 -100000 Long/MIN_VALUE])
     (dt-test-cases/true-case "any negative float"
       :samples [(float -0.00000000000001)
                 (float -1.23) (float -35.456) (float -100000)
                 (- Float/MIN_VALUE)
                 (- Float/MAX_VALUE)])
     (dt-test-cases/true-case "any negative double"
       :samples [-0.000000000000000000001
                 -1.23 -35.456 -100000
                 (- Double/MIN_VALUE)
                 (- Double/MAX_VALUE)])
     (dt-test-cases/true-case "any negative big int"
       :samples [(biginteger -1) (biginteger -35) (biginteger -100000)
                 (bigint "-111111111111111999999999999991111111")])
     (dt-test-cases/true-case "any negative big integer"
       :samples [-1N -35N -100000N
                 (biginteger "-111111111111111999999999999991111111")])
     (dt-test-cases/true-case "any negative big decimal"
       :samples [(bigdec -1.123) (bigdec -35.2) (bigdec -100000)
                 (bigdec "-111111111111111999999999999991111111.4567")])
     (dt-test-cases/false-case "any zero value number"
       :samples [(int 0) 0 (float 0) 0.0 0M 0N (biginteger 0)])
     (dt-test-cases/false-case "any positive int"
       :samples [(int 1) (int 35) (int 100000) Integer/MAX_VALUE])
     (dt-test-cases/false-case "any positive long"
       :samples [1 35 100000 Long/MAX_VALUE])
     (dt-test-cases/false-case "any positive float"
       :samples [(float 0.00000000000001)
                 (float 1.23) (float 35.456) (float 100000)
                 Float/MIN_VALUE
                 Float/MAX_VALUE])
     (dt-test-cases/false-case "any positive double"
       :samples [0.000000000000000000001
                 1.23 35.456 100000
                 Double/MIN_VALUE
                 Double/MAX_VALUE])
     (dt-test-cases/false-case "any positive big int"
       :samples [1N 35N 100000N
                 (bigint "111111111111111999999999999991111111")])
     (dt-test-cases/false-case "any positive big integer"
       :samples [(biginteger 1) (biginteger 35) (biginteger 100000)
                 (biginteger "111111111111111999999999999991111111")])
     (dt-test-cases/false-case "any positive big decimal"
       :samples [(bigdec 1.123) (bigdec 35.2) (bigdec 100000)
                 (bigdec "111111111111111999999999999991111111.4567")])
     (dt-test-cases/false-case "any number as a string"
       :samples ["1" "-1" "100.2" "-100.45"])
     (dt-test-cases/false-case "a non-number"
       :samples [true false "not-an-integer"])
     (dt-test-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sd-number/negative?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest negative?-as-requirement
  (is (= :must-be-a-negative-number
        (sv-core/pred-requirement
          'spec.definition.number.core/negative?))))

(deftest clojure-core-neg?-as-requirement
  (is (= :must-be-a-negative-number
        (sv-core/pred-requirement
          'clojure.core/neg?))))

(deftest clojure-core-neg-int?-as-requirement
  (is (= :must-be-a-negative-integer
        (sv-core/pred-requirement
          'clojure.core/neg-int?))))

(deftest negative?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (spec/gen sd-number/negative?) 100)))))
  (testing "generates negative numbers"
    (is (every? true?
          (map #(clojure.core/neg? %)
            (gen/sample (spec/gen sd-number/negative?)))))))

(deftest zero?-as-predicate
  (doseq
   [case
    [(dt-test-cases/true-case "any zero value number"
       :samples [(int 0) 0 (float 0.0) 0.0 0M 0N (biginteger 0)])
     (dt-test-cases/false-case "any negative integer"
       :samples [(int -1) (int -35) (int -100000) Integer/MIN_VALUE])
     (dt-test-cases/false-case "any negative long"
       :samples [-1 -35 -100000 Long/MIN_VALUE])
     (dt-test-cases/false-case "any negative float"
       :samples [(float -0.00000000000001)
                 (float -1.23) (float -35.456) (float -100000)
                 (- Float/MIN_VALUE)
                 (- Float/MAX_VALUE)])
     (dt-test-cases/false-case "any negative double"
       :samples [-0.000000000000000000001
                 -1.23 -35.456 -100000
                 (- Double/MIN_VALUE)
                 (- Double/MAX_VALUE)])
     (dt-test-cases/false-case "any negative big int"
       :samples [(biginteger -1) (biginteger -35) (biginteger -100000)
                 (bigint "-111111111111111999999999999991111111")])
     (dt-test-cases/false-case "any negative big integer"
       :samples [-1N -35N -100000N
                 (biginteger "-111111111111111999999999999991111111")])
     (dt-test-cases/false-case "any negative big decimal"
       :samples [(bigdec -1.123) (bigdec -35.2) (bigdec -100000)
                 (bigdec "-111111111111111999999999999991111111.4567")])
     (dt-test-cases/false-case "any positive int"
       :samples [(int 1) (int 35) (int 100000) Integer/MAX_VALUE])
     (dt-test-cases/false-case "any positive long"
       :samples [1 35 100000 Long/MAX_VALUE])
     (dt-test-cases/false-case "any positive float"
       :samples [(float 0.00000000000001)
                 (float 1.23) (float 35.456) (float 100000)
                 Float/MIN_VALUE
                 Float/MAX_VALUE])
     (dt-test-cases/false-case "any positive double"
       :samples [0.000000000000000000001
                 1.23 35.456 100000
                 Double/MIN_VALUE
                 Double/MAX_VALUE])
     (dt-test-cases/false-case "any positive big int"
       :samples [1N 35N 100000N
                 (bigint "111111111111111999999999999991111111")])
     (dt-test-cases/false-case "any positive big integer"
       :samples [(biginteger 1) (biginteger 35) (biginteger 100000)
                 (biginteger "111111111111111999999999999991111111")])
     (dt-test-cases/false-case "any positive big decimal"
       :samples [(bigdec 1.123) (bigdec 35.2) (bigdec 100000)
                 (bigdec "111111111111111999999999999991111111.4567")])
     (dt-test-cases/false-case "any number as a string"
       :samples ["1" "-1" "100.2" "-100.45"])
     (dt-test-cases/false-case "a non-number"
       :samples [true false "not-an-integer"])
     (dt-test-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sd-number/zero?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest zero?-as-requirement
  (is (= :must-be-zero
        (sv-core/pred-requirement
          'spec.definition.number.core/zero?))))

(deftest clojure-core-zero?-as-requirement
  (is (= :must-be-zero
        (sv-core/pred-requirement
          'clojure.core/zero?))))

(deftest zero?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (spec/gen sd-number/zero?) 100)))))
  (testing "generates zero numbers"
    (is (every? true?
          (map #(= 0 %)
            (gen/sample (spec/gen sd-number/zero?)))))))
