(ns spec.validate.currency-test
  (:require
    [clojure.test :refer [deftest is testing]]

    [spec.validate.currency :as sv-currency]))

(deftest for-currency-amount?
  (testing "returns true when provided string represents a currency amount"
    (let [target "10.20"]
      (is (true? (sv-currency/currency-amount? target)))))

  (testing
    "returns false when provided string does not represent a currency amount"
    (let [target "the quick brown fox"]
      (is (false? (sv-currency/currency-amount? target)))))

  (testing "returns false when provided value is not a string"
    (let [target true]
      (is (false? (sv-currency/currency-amount? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (sv-currency/currency-amount? target))))))

(deftest for-currency-code?
  (testing "returns true when provided string represents a currency code"
    (let [target "JPY"]
      (is (true? (sv-currency/currency-code? target)))))

  (testing
    "returns false when provided string does not represent a currency code"
    (let [target "the quick brown fox"]
      (is (false? (sv-currency/currency-code? target)))))

  (testing "returns false when provided value is not a string"
    (let [target true]
      (is (false? (sv-currency/currency-code? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (sv-currency/currency-code? target))))))
