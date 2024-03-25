(ns spec.validate.number-test
  (:require
    [clojure.test :refer [deftest is testing]]

    [spec.validate.number :as sv-number]))

(deftest for-integer?
  (testing "returns true when provided value is an integer"
    (let [target 3]
      (is (true? (sv-number/integer? target)))))

  (testing "returns false when provided value is a decimal"
    (let [target 3.14]
      (is (false? (sv-number/integer? target)))))

  (testing "returns false when provided value is a string"
    (let [target "3"]
      (is (false? (sv-number/integer? target)))))

  (testing "returns false when provided value is a boolean"
    (let [target true]
      (is (false? (sv-number/integer? target))))))

(deftest for-positive?
  (testing "returns true when provided string represents a positive number"
    (let [target "54.22"]
      (is (true? (sv-number/positive? target)))))

  (testing "returns true when provided value is a positive number"
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

  (testing "returns true when provided value is a negative number"
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
