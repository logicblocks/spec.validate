(ns spec.validate.number-test
  (:require
   [clojure.spec.alpha :as spec]
   [clojure.spec.gen.alpha :as gen]
   [clojure.test :refer [deftest is testing]]

   [spec.validate.core :as sv-core]
   [spec.validate.number :as sv-number]

   [spec.validate.test-support.cases :as sv-cases]
   [spec.validate.test-support.string :as sv-string]
   [spec.validate.utils :as sv-utils]))

(deftest integer?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any integer"
       :samples [0 35 -35
                 0xff 017 2r1011 7N 36rCRAZY
                 Integer/MIN_VALUE
                 Integer/MAX_VALUE])
     (sv-cases/false-case "a decimal" :sample 35.23)
     (sv-cases/false-case "a non-integer"
       :samples [true false "not-an-integer"])
     (sv-cases/false-case "an integer string" :sample "12345")
     (sv-cases/false-case "nil" :sample nil)]]
    (testing (str "for " (:title case))
      (is (every?
            #(= (sv-number/integer? %) (:satisfied? case))
            (:samples case))))))

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
       :sample "00123")
     (sv-cases/false-case "any integer"
       :samples [0 35 -35
                 0xff 017 2r1011 7N 36rCRAZY
                 Integer/MIN_VALUE
                 Integer/MAX_VALUE])
     (sv-cases/false-case "a decimal string" :sample "35.23")
     (sv-cases/false-case "a decimal" :sample 35.23)
     (sv-cases/false-case "a non-numeric string"
       :sample "not-an-integer")
     (sv-cases/false-case "a non-string"
       :samples [true false #{1 2 3}])
     (sv-cases/false-case "an empty string"
       :sample "")
     (sv-cases/false-case "nil" :sample nil)]]
    (testing (str "for " (:title case))
      (is (every?
            #(= (sv-number/integer-string? %) (:satisfied? case))
            (:samples case))
        (str "the following samples were unsatisfactory: "
          (filterv #(not (= (sv-number/integer-string? %) (:satisfied? case)))
            (:samples case)))))))

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
