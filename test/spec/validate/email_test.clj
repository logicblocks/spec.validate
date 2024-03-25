(ns spec.validate.email-test
  (:require
    [clojure.test :refer [deftest is testing]]

    [spec.validate.email :as sv-email]))

(deftest for-email-address?
  (testing "returns true when provided string is a valid email address"
    (let [target "person@example.com"]
      (is (true? (sv-email/email-address? target)))))

  (testing
    "returns false when provided string does not represent an email address"
    (let [target "the quick brown fox"]
      (is (false? (sv-email/email-address? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 35]
      (is (false? (sv-email/email-address? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (sv-email/email-address? target))))))
