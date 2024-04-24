(ns datatype.number.core-test
  (:require
   [clojure.test :refer [deftest testing]]

   [datatype.number.core :as dt-number]

   [spec.test-support.cases :as sv-cases])
  (:import [java.util Locale]))

(deftest integer-string?
  (testing "general cases"
    (sv-cases/assert-cases-satisfied-by dt-number/integer-string?
      (sv-cases/true-case "any integer as a string"
        :samples ["0" "-0" "+0" "35" "-35" "+35"
                  (str Integer/MIN_VALUE)
                  (str Integer/MAX_VALUE)])
      (sv-cases/false-case "any non-base 10 integer as a string"
        :samples ["0xff" "035" "2r1011" "7N" "36rCRAZY"])
      (sv-cases/false-case "many zeroes"
        :samples ["00", "0000000"])
      (sv-cases/false-case "leading zeroes"
        :samples ["00123" "-00123"])
      (sv-cases/false-case "multiple leading signs"
        :samples ["+-123" "-+45" "--0" "++10" "+-+23"])
      (sv-cases/false-case "any integer"
        :samples [0 35 -35
                  0xff 017 2r1011 7N 36rCRAZY
                  Integer/MIN_VALUE
                  Integer/MAX_VALUE])
      (sv-cases/false-case "a decimal" :sample 35.23)
      (sv-cases/false-case "a non-numeric string"
        :sample "not-an-integer")
      (sv-cases/false-case "a non-string"
        :samples [true false #{1 2 3}])
      (sv-cases/false-case "an empty string"
        :sample "")
      (sv-cases/false-case "nil" :sample nil)))

  (testing "locale specific cases"
    (sv-cases/assert-cases-satisfied-by dt-number/integer-string?
      (sv-cases/true-case
        "an integer string with correct thousands separator for the UK"
        :samples ["1,000" "10,000" "100,000" "1,000,000"]
        :locale (Locale/UK))
      (sv-cases/false-case
        "an integer string with incorrect thousands separator for the UK"
        :samples ["1.000" "10.000" "100.000" "1.000.000"]
        :locale (Locale/UK))
      (sv-cases/true-case
        "an integer string with correct thousands separator for Germany"
        :samples ["1.000" "10.000" "100.000" "1.000.000"]
        :locale (Locale/GERMANY))
      (sv-cases/false-case
        "an integer string with incorrect thousands separator for Germany"
        :samples ["1,000" "10,000" "100,000" "1,000,000"]
        :locale (Locale/GERMANY))
      (sv-cases/true-case
        "an integer string with thousands separator in the wrong place"
        :samples ["1,0" "1,00,0000"]
        :locale (Locale/UK)
        :note "TODO: This is treated as valid but shouldn't be...")
      (sv-cases/false-case "a decimal string"
        :sample "35.23"
        :locale (Locale/UK)))))

(deftest decimal-string?
  (testing "general cases"
    (sv-cases/assert-cases-satisfied-by dt-number/decimal-string?
      (sv-cases/true-case "any decimal as a string"
        :samples ["0" "-0" "+0" "0." "-0." "+0."
                  "0.00" "-0.0000" "+0.000"
                  "0.82" "+0.82" "-0.82"
                  "35." "-35." "+35." "35" "-35" "+35"
                  "2.78" ".91" "+.72"])
      (sv-cases/false-case "any non-base 10 integer as a string"
        :samples ["0xff" "035" "2r1011" "7N" "36rCRAZY"])
      (sv-cases/false-case "many zeroes"
        :samples ["00", "0000000"])
      (sv-cases/false-case "leading zeroes"
        :samples ["00123" "00.123" "000.456"])
      (sv-cases/false-case "multiple leading signs"
        :samples ["+-123.4" "-+45" "--0.23" "++10.0" "+-+23.456"])
      (sv-cases/false-case "any integer"
        :samples [0 35 -35
                  0xff 017 2r1011 7N 36rCRAZY
                  Integer/MIN_VALUE
                  Integer/MAX_VALUE])
      (sv-cases/false-case "a floating point number" :sample 35.23)
      (sv-cases/false-case "a non-numeric string"
        :sample "not-a-decimal")
      (sv-cases/false-case "a non-string"
        :samples [true false #{1 2 3}])
      (sv-cases/false-case "an empty string"
        :sample "")
      (sv-cases/false-case "nil" :sample nil)))

  (testing "locale specific cases"
    (sv-cases/assert-cases-satisfied-by dt-number/decimal-string?
      (sv-cases/true-case
        "a decimal string with correct thousands separator for the UK"
        :samples ["1,000.456" "10,000" "100,000.1" "1,000,000."]
        :locale (Locale/UK))
      (sv-cases/false-case
        "a decimal string with incorrect thousands separator for the UK"
        :samples ["1.000,56" "10.000," "100.000,1" "1.000.000,"]
        :locale (Locale/UK))
      (sv-cases/true-case
        "a decimal string with correct thousands separator for Germany"
        :samples ["1.000,123" "10.000" "100.000,4" "1.000.000,"]
        :locale (Locale/GERMANY))
      (sv-cases/false-case
        "a decimal string with incorrect thousands separator for Germany"
        :samples ["1,000.456" "10,000.0" "100,000.1" "1,000,000."]
        :locale (Locale/GERMANY))
      (sv-cases/true-case
        "a decimal string with correct thousands separator for France"
        :samples ["1 000,456"
                  "10 000,0"
                  "100 000,1"
                  "1 000 000,"]
        :locale (Locale/FRANCE))
      (sv-cases/false-case
        "a decimal string with incorrect thousands separator for France"
        :samples ["1,000.123" "10.000,0" "100,000.4" "1.000.000,"]
        :locale (Locale/FRANCE))
      (sv-cases/true-case
        "a decimal string with thousands separator in the wrong place"
        :samples ["1,0.1" "1,00,0000."]
        :locale (Locale/UK)
        :note "TODO: This is treated as valid but shouldn't be..."))))
