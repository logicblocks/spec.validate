(ns spec.validate.boolean-test
  (:require
    [clojure.test :refer [deftest is testing]]

    [spec.validate.boolean :as sv-boolean]))

(deftest for-boolean?
  (testing "returns true when provided value is a string"
    (let [target true]
      (is (true? (sv-boolean/boolean? target)))))

  (testing "returns false when provided value is not a string"
    (let [target "string"]
      (is (false? (sv-boolean/boolean? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (sv-boolean/boolean? target))))))
