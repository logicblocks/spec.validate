(ns spec.validate.identifier-test
  (:require
    [clojure.test :refer [deftest is testing]]

    [spec.validate.identifier :as sv-identifier]))

(deftest for-uuid-v4?
  (testing "returns true when provided string represents a lower case v4 UUID"
    (let [target "2571f835-bb47-4637-a0ed-dbfc82583f7d"]
      (is (true? (sv-identifier/uuid-v4? target)))))

  (testing "returns true when provided string represents a upper case v4 UUID"
    (let [target "2571F835-BB47-4637-A0ED-DBFC82583F7D"]
      (is (true? (sv-identifier/uuid-v4? target)))))

  (testing "returns false when provided string is not a v4 UUID"
    (let [target "69f7a4cc-79a0-11e9-8f9e-2a86e4085a59"]
      (is (false? (sv-identifier/uuid-v4? target)))))

  (testing "returns false when provided string is not a UUID"
    (let [target "the quick brown fox"]
      (is (false? (sv-identifier/uuid-v4? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 25]
      (is (false? (sv-identifier/uuid-v4? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (sv-identifier/uuid-v4? target))))))
