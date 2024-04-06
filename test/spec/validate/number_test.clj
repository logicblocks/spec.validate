(ns spec.validate.number-test
  (:require
   [clojure.spec.alpha :as spec]
   [clojure.spec.gen.alpha :as gen]
   [clojure.test :refer [deftest is testing]]

   [spec.validate.core :as sv-core]
   [spec.validate.number :as sv-number]

   [spec.validate.test-support.cases :as sv-cases]
   [spec.validate.utils :as sv-utils])
  (:import
   [java.text DecimalFormat]
   [java.util Locale]
   [java.util.regex Pattern]))

(defmacro with-locale [^Locale locale & body]
  `(let [default-locale# (Locale/getDefault)]
     (try
       (Locale/setDefault ~locale)
       ~@body
       (finally
         (Locale/setDefault default-locale#)))))

(deftest integer?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any integer"
       :samples [0 -0 +0 35 -35 +35
                 0xff 017 2r1011 7N 36rCRAZY
                 Integer/MIN_VALUE
                 Integer/MAX_VALUE])
     (sv-cases/false-case "a decimal" :sample 35.23)
     (sv-cases/false-case "a non-integer"
       :samples [true false "not-an-integer"])
     (sv-cases/false-case "an integer string" :sample "12345")
     (sv-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sv-number/integer?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest integer?-as-requirement
  (is (= :must-be-an-integer
        (sv-core/pred-requirement
          'spec.validate.number/integer?)))
  (is (= :must-be-an-integer
        (sv-core/pred-requirement
          'clojure.core/integer?))))

(deftest integer?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (spec/gen sv-number/integer?) 100)))))
  (testing "generates integers"
    (is (every? true?
          (map #(clojure.core/integer? %)
            (gen/sample (spec/gen sv-number/integer?)))))))

(deftest integer-string?-as-predicate
  (testing "general cases"
    (doseq
     [case
      [(sv-cases/true-case "any integer as a string"
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
       (sv-cases/false-case "nil" :sample nil)]]
      (let [{:keys [samples satisfied? title]} case
            pred sv-number/integer-string?]
        (testing (str "for " title)
          (is (every? #(= % satisfied?) (map pred samples))
            (str "unsatisfied for: "
              (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

  (testing "locale specific cases"
    (doseq
     [case
      [(sv-cases/true-case
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
         :locale (Locale/UK))]]
      (let [{:keys [samples satisfied? title locale]} case
            pred sv-number/integer-string?]
        (testing (str "for " title)
          (with-locale locale
            (is (every? #(= % satisfied?) (map pred samples))
              (str "unsatisfied for: "
                (into #{} (filter #(not (= (pred %) satisfied?))
                            samples))))))))))

(deftest integer-string?-as-requirement
  (is (= :must-be-an-integer-string
        (sv-core/pred-requirement
          'spec.validate.number/integer-string?))))

(deftest integer-string?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (spec/gen sv-number/integer-string?) 100)))))

  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample (spec/gen sv-number/integer-string?) 100)))))

  (testing "generates integer strings"
    (let [samples (gen/sample (spec/gen sv-number/integer-string?))
          pred #(or (= "0" %) (sv-utils/re-satisfies? #"^[-+]?[1-9]\d*$" %))]
      (is (every? true? (map pred samples))
        (str "mismatching samples: "
          (filterv #(not (pred %)) samples))))))

(deftest floating-point-number?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any floating point literal"
       :samples [0. -0. +0. 35. -35. +35. 2.78 0.82 -1.2e5 +1.2e5])
     (sv-cases/true-case "any boxed floating point number"
       :samples [(Double/parseDouble "1.23") (Float/parseFloat "3.45")])
     (sv-cases/true-case "any special case floating point value"
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
     (sv-cases/false-case "an integer literal"
       :samples [0 35])
     (sv-cases/false-case "a non-number"
       :samples [true false "not-a-floating-point-number"])
     (sv-cases/false-case "an decimal string" :sample "12345")
     (sv-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sv-number/floating-point-number?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest floating-point-number?-as-requirement
  (is (= :must-be-a-floating-point-number
        (sv-core/pred-requirement
          'spec.validate.number/floating-point-number?))))

(deftest floating-point-number?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (spec/gen sv-number/floating-point-number?) 100)))))

  (testing "generates floating point numbers"
    (let [samples (gen/sample (spec/gen sv-number/floating-point-number?))
          pred float?]
      (is (every? true? (map pred samples))
        (str "mismatching samples: "
          (into #{} (filterv #(not (pred %)) samples)))))))

(deftest decimal-string?-as-predicate
  (testing "general cases"
    (doseq
     [case
      [(sv-cases/true-case "any decimal as a string"
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
       (sv-cases/false-case "nil" :sample nil)]]
      (let [{:keys [samples satisfied? title locale]} case
            pred sv-number/decimal-string?]
        (testing (str "for " title)
          (with-locale locale
            (is (every? #(= % satisfied?) (map pred samples))
              (str "unsatisfied for: "
                (into #{}
                  (filter #(not (= (pred %) satisfied?)) samples)))))))))

  (testing "locale specific cases"
    (doseq
     [case
      [(sv-cases/true-case
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
         :note "TODO: This is treated as valid but shouldn't be...")]]
      (let [{:keys [samples satisfied? title locale]} case
            pred sv-number/decimal-string?]
        (testing (str "for " title)
          (with-locale locale
            (is (every? #(= % satisfied?) (map pred samples))
              (str "unsatisfied for: "
                (into #{} (filter #(not (= (pred %) satisfied?))
                            samples))))))))))

(deftest decimal-string?-as-requirement
  (is (= :must-be-a-decimal-string
        (sv-core/pred-requirement
          'spec.validate.number/decimal-string?))))

(deftest decimal-string?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (spec/gen sv-number/decimal-string?) 100)))))

  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample (spec/gen sv-number/decimal-string?) 100)))))

  (testing "generates decimal strings"
    (let [samples (gen/sample (spec/gen sv-number/decimal-string?))
          can-parse?
          (fn [x]
            (try
              (boolean (parse-double x))
              (catch Exception _ false)))]
      (is (every? true? (map can-parse? samples))
        (str "mismatching samples: "
          (filterv #(not (can-parse? %)) samples))))))

(deftest for-positive?
  (testing "returns true when provided string represents a positive number"
    (let [target "54.22"]
      (is (true? (sv-number/positive? target)))))

  #_(testing "returns true when provided value is a positive number"
      (let [target 54.22]
        (is (true? (sv-number/positive? target)))))

  (testing "returns false when provided string represents a zero"
    (let [target "0.00"]
      (is (false? (sv-number/positive? target)))))

  (testing "returns false when provided value is zero"
    (let [target 0.00]
      (is (false? (sv-number/positive? target)))))

  (testing "returns false when provided string represents a negative number"
    (let [target "-52.30"]
      (is (false? (sv-number/positive? target)))))

  (testing "returns false when provided value is a negative number"
    (let [target -18]
      (is (false? (sv-number/positive? target)))))

  (testing "returns false when provided value is not a string"
    (let [target true]
      (is (false? (sv-number/positive? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (sv-number/positive? target))))))

(deftest for-negative?
  (testing "returns true when provided string represents a negative number"
    (let [target "-54.22"]
      (is (true? (sv-number/negative? target)))))

  #_(testing "returns true when provided value is a negative number"
      (let [target -54.22]
        (is (true? (sv-number/negative? target)))))

  (testing "returns false when provided string represents a zero"
    (let [target "0.00"]
      (is (false? (sv-number/negative? target)))))

  (testing "returns false when provided value is zero"
    (let [target 0.00]
      (is (false? (sv-number/negative? target)))))

  (testing "returns false when provided string represents a negative number"
    (let [target "52.30"]
      (is (false? (sv-number/negative? target)))))

  (testing "returns false when provided value is a negative number"
    (let [target 18]
      (is (false? (sv-number/negative? target)))))

  (testing "returns false when provided value is not a string"
    (let [target true]
      (is (false? (sv-number/negative? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (sv-number/negative? target))))))

(deftest for-zero?
  (testing "returns true when provided string represents zero"
    (let [target "0.00"]
      (is (true? (sv-number/zero? target)))))

  (testing "returns false when provided string does not represents zero"
    (let [target "12.22"]
      (is (false? (sv-number/zero? target)))))

  (testing "returns false when provided value is not a string"
    (let [target true]
      (is (false? (sv-number/zero? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (sv-number/zero? target))))))
