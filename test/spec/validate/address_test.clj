(ns spec.validate.address-test
  (:require
   [clojure.test :refer [deftest testing is]]

   [spec.validate.address :as sv-address]))

(deftest for-postcode?
  (testing "returns true when provided string represents a UK postcode"
    (let [target "EC1A 1BB"]
      (is (true? (sv-address/postcode? target)))))

  (testing "returns false when provided string does not represent a UK postcode"
    (let [target "the quick brown fox"]
      (is (false? (sv-address/postcode? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 35]
      (is (false? (sv-address/postcode? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (sv-address/postcode? target))))))
