(ns spec.validate.phone-test
  (:require
    [clojure.test :refer [deftest is testing]]

    [spec.validate.phone :as sv-phone]))

(deftest for-phone-number?
  (testing "returns true when provided string represents a phone number"
    (let [target "07890424158"]
      (is (true? (sv-phone/phone-number? target)))))

  (testing
    "returns false when provided string does not represent a phone number"
    (let [target "the quick brown fox"]
      (is (false? (sv-phone/phone-number? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 35]
      (is (false? (sv-phone/phone-number? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (sv-phone/phone-number? target))))))
