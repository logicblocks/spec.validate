(ns spec.validate.collection-test
  (:require
    [clojure.test :refer [deftest is testing]]

    [spec.validate.collection :as sv-collection]))

(deftest for-not-empty?
  (testing "returns true when provided collection has many items"
    (let [target ["first" "second" "third"]]
      (is (true? (sv-collection/not-empty? target)))))

  (testing "returns true when provided collection has one item"
    (let [target ["first"]]
      (is (true? (sv-collection/not-empty? target)))))

  (testing "returns false when provided collection is empty"
    (let [target []]
      (is (false? (sv-collection/not-empty? target)))))

  (testing "returns false when provided value is not a collection"
    (let [target true]
      (is (false? (sv-collection/not-empty? target)))))

  (testing "returns false when provided value is nil"
    (let [target true]
      (is (false? (sv-collection/not-empty? target))))))

(deftest for-length-equal-to?
  (testing "returns a validator that"
    (testing "returns true when the provided string has the specified length"
      (let [target "abcdef"
            length-equal-to-6? (sv-collection/length-equal-to? 6)]
        (is (true? (length-equal-to-6? target)))))

    (testing "returns false when the provided string has a different length"
      (let [target "abcdefgh"
            length-equal-to-6? (sv-collection/length-equal-to? 6)]
        (is (false? (length-equal-to-6? target)))))

    (testing "returns false when the provided value is not a string"
      (let [target 25
            length-equal-to-6? (sv-collection/length-equal-to? 6)]
        (is (false? (length-equal-to-6? target)))))

    (testing "returns false when the provided value is nil"
      (let [target nil
            length-equal-to-6? (sv-collection/length-equal-to? 6)]
        (is (false? (length-equal-to-6? target)))))))

(deftest for-length-less-than?
  (testing "returns a validator that"
    (testing
      "returns true when the provided string has length less than specified"
      (let [target "abcdef"
            length-less-than-10? (sv-collection/length-less-than? 10)]
        (is (true? (length-less-than-10? target)))))

    (testing "returns false when the provided string length equal to specified"
      (let [target "abcdefghij"
            length-less-than-10? (sv-collection/length-less-than? 10)]
        (is (false? (length-less-than-10? target)))))

    (testing
      "returns false when the provided string length greater than specified"
      (let [target "abcdefghijkl"
            length-less-than-10? (sv-collection/length-less-than? 10)]
        (is (false? (length-less-than-10? target)))))

    (testing "returns false when the provided value is not a string"
      (let [target 25
            length-less-than-10? (sv-collection/length-less-than? 10)]
        (is (false? (length-less-than-10? target)))))

    (testing "returns false when the provided value is nil"
      (let [target nil
            length-less-than-10? (sv-collection/length-less-than? 10)]
        (is (false? (length-less-than-10? target)))))))

(deftest for-length-greater-than?
  (testing "returns a validator that"
    (testing
      "returns true when the provided string has length greater than specified"
      (let [target "abcdefghijk"
            length-greater-than-10? (sv-collection/length-greater-than? 10)]
        (is (true? (length-greater-than-10? target)))))

    (testing "returns false when the provided string length equal to specified"
      (let [target "abcdefghij"
            length-greater-than-10? (sv-collection/length-greater-than? 10)]
        (is (false? (length-greater-than-10? target)))))

    (testing
      "returns false when the provided string length less than specified"
      (let [target "abcdef"
            length-greater-than-10? (sv-collection/length-greater-than? 10)]
        (is (false? (length-greater-than-10? target)))))

    (testing "returns false when the provided value is not a string"
      (let [target 25
            length-greater-than-10? (sv-collection/length-greater-than? 10)]
        (is (false? (length-greater-than-10? target)))))

    (testing "returns false when the provided value is nil"
      (let [target nil
            length-greater-than-10? (sv-collection/length-greater-than? 10)]
        (is (false? (length-greater-than-10? target)))))))
