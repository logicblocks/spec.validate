(ns spec.validate.time-test
  (:require
    [clojure.test :refer [deftest is testing]]

    [spec.validate.time :as sv-time]))

(deftest for-iso8601-datetime?
  (testing "returns true when provided string is an ISO8601 datetime"
    (let [target "2019-01-01T12:00:00Z"]
      (is (true? (sv-time/iso8601-datetime? target)))))

  (testing "returns false when provided string is not an ISO8601 datetime"
    (let [target "the quick brown fox"]
      (is (false? (sv-time/iso8601-datetime? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 35]
      (is (false? (sv-time/iso8601-datetime? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (sv-time/iso8601-datetime? target))))))
