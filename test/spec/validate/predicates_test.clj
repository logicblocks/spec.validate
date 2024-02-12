(ns spec.validate.predicates-test
  (:refer-clojure :exclude [boolean? integer? uuid? zero? string?])
  (:require
   [clojure.test :refer :all]

   [spec.validate.predicates :as preds]))

;; identifiers
(deftest for-uuid-v4?
  (testing "returns true when provided string represents a lower case v4 UUID"
    (let [target "2571f835-bb47-4637-a0ed-dbfc82583f7d"]
      (is (true? (preds/uuid-v4? target)))))

  (testing "returns true when provided string represents a upper case v4 UUID"
    (let [target "2571F835-BB47-4637-A0ED-DBFC82583F7D"]
      (is (true? (preds/uuid-v4? target)))))

  (testing "returns false when provided string is not a v4 UUID"
    (let [target "69f7a4cc-79a0-11e9-8f9e-2a86e4085a59"]
      (is (false? (preds/uuid-v4? target)))))

  (testing "returns false when provided string is not a UUID"
    (let [target "the quick brown fox"]
      (is (false? (preds/uuid-v4? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 25]
      (is (false? (preds/uuid-v4? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (preds/uuid-v4? target))))))

;; URLs
(deftest for-absolute-url?
  (testing "returns true when provided string represents an absolute URL"
    (let [target "https://example.com/some/path/to/file.html"]
      (is (true? (preds/absolute-url? target)))))

  (testing "returns false when provided string is not absolute"
    (let [target "/some/path/to/file.html"]
      (is (false? (preds/absolute-url? target)))))

  (testing "returns false when provided string is not a url"
    (let [target "the quick brown fox"]
      (is (false? (preds/absolute-url? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 35]
      (is (false? (preds/absolute-url? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (preds/absolute-url? target))))))

;; boolean
(deftest for-boolean?
  (testing "returns true when provided value is a string"
    (let [target true]
      (is (true? (preds/boolean? target)))))

  (testing "returns false when provided value is not a string"
    (let [target "string"]
      (is (false? (preds/boolean? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (preds/boolean? target))))))

(deftest for-content?
  (testing "returns true when provided string has content"
    (let [target "stuff"]
      (is (true? (preds/content? target)))))

  (testing "returns false when provided string is only whitespace"
    (let [target "  "]
      (is (false? (preds/content? target)))))

  (testing "returns false when provided string is empty"
    (let [target ""]
      (is (false? (preds/content? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 35]
      (is (false? (preds/content? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (preds/content? target))))))

(deftest for-digits?
  (testing "returns true when provided string contains only numbers"
    (let [target "123456789"]
      (is (true? (preds/digits? target)))))

  (testing "returns false when provided string contains other characters"
    (let [target "12ab34cd56ef78"]
      (is (false? (preds/digits? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 35]
      (is (false? (preds/digits? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (preds/digits? target))))))

;; numbers
(deftest for-integer?
  (testing "returns true when provided value is an integer"
    (let [target 3]
      (is (true? (preds/integer? target)))))

  (testing "returns false when provided value is a decimal"
    (let [target 3.14]
      (is (false? (preds/integer? target)))))

  (testing "returns false when provided value is a string"
    (let [target "3"]
      (is (false? (preds/integer? target)))))

  (testing "returns false when provided value is a boolean"
    (let [target true]
      (is (false? (preds/integer? target))))))

(deftest for-positive?
  (testing "returns true when provided string represents a positive number"
    (let [target "54.22"]
      (is (true? (preds/positive? target)))))

  (testing "returns true when provided value is a positive number"
    (let [target 54.22]
      (is (true? (preds/positive? target)))))

  (testing "returns false when provided string represents a zero"
    (let [target "0.00"]
      (is (false? (preds/positive? target)))))

  (testing "returns false when provided value is zero"
    (let [target 0.00]
      (is (false? (preds/positive? target)))))

  (testing "returns false when provided string represents a negative number"
    (let [target "-52.30"]
      (is (false? (preds/positive? target)))))

  (testing "returns false when provided value is a negative number"
    (let [target -18]
      (is (false? (preds/positive? target)))))

  (testing "returns false when provided value is not a string"
    (let [target true]
      (is (false? (preds/positive? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (preds/positive? target))))))

(deftest for-negative?
  (testing "returns true when provided string represents a negative number"
    (let [target "-54.22"]
      (is (true? (preds/negative? target)))))

  (testing "returns true when provided value is a negative number"
    (let [target -54.22]
      (is (true? (preds/negative? target)))))

  (testing "returns false when provided string represents a zero"
    (let [target "0.00"]
      (is (false? (preds/negative? target)))))

  (testing "returns false when provided value is zero"
    (let [target 0.00]
      (is (false? (preds/negative? target)))))

  (testing "returns false when provided string represents a negative number"
    (let [target "52.30"]
      (is (false? (preds/negative? target)))))

  (testing "returns false when provided value is a negative number"
    (let [target 18]
      (is (false? (preds/negative? target)))))

  (testing "returns false when provided value is not a string"
    (let [target true]
      (is (false? (preds/negative? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (preds/negative? target))))))

(deftest for-zero?
  (testing "returns true when provided string represents zero"
    (let [target "0.00"]
      (is (true? (preds/zero? target)))))

  (testing "returns false when provided string does not represents zero"
    (let [target "12.22"]
      (is (false? (preds/zero? target)))))

  (testing "returns false when provided value is not a string"
    (let [target true]
      (is (false? (preds/zero? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (preds/zero? target))))))

;; collections
(deftest for-not-empty?
  (testing "returns true when provided collection has many items"
    (let [target ["first" "second" "third"]]
      (is (true? (preds/not-empty? target)))))

  (testing "returns true when provided collection has one item"
    (let [target ["first"]]
      (is (true? (preds/not-empty? target)))))

  (testing "returns false when provided collection is empty"
    (let [target []]
      (is (false? (preds/not-empty? target)))))

  (testing "returns false when provided value is not a collection"
    (let [target true]
      (is (false? (preds/not-empty? target)))))

  (testing "returns false when provided value is nil"
    (let [target true]
      (is (false? (preds/not-empty? target))))))

(deftest for-length-equal-to?
  (testing "returns a validator that"
    (testing "returns true when the provided string has the specified length"
      (let [target "abcdef"
            length-equal-to-6? (preds/length-equal-to? 6)]
        (is (true? (length-equal-to-6? target)))))

    (testing "returns false when the provided string has a different length"
      (let [target "abcdefgh"
            length-equal-to-6? (preds/length-equal-to? 6)]
        (is (false? (length-equal-to-6? target)))))

    (testing "returns false when the provided value is not a string"
      (let [target 25
            length-equal-to-6? (preds/length-equal-to? 6)]
        (is (false? (length-equal-to-6? target)))))

    (testing "returns false when the provided value is nil"
      (let [target nil
            length-equal-to-6? (preds/length-equal-to? 6)]
        (is (false? (length-equal-to-6? target)))))))

(deftest for-length-less-than?
  (testing "returns a validator that"
    (testing
     "returns true when the provided string has length less than specified"
      (let [target "abcdef"
            length-less-than-10? (preds/length-less-than? 10)]
        (is (true? (length-less-than-10? target)))))

    (testing "returns false when the provided string length equal to specified"
      (let [target "abcdefghij"
            length-less-than-10? (preds/length-less-than? 10)]
        (is (false? (length-less-than-10? target)))))

    (testing
     "returns false when the provided string length greater than specified"
      (let [target "abcdefghijkl"
            length-less-than-10? (preds/length-less-than? 10)]
        (is (false? (length-less-than-10? target)))))

    (testing "returns false when the provided value is not a string"
      (let [target 25
            length-less-than-10? (preds/length-less-than? 10)]
        (is (false? (length-less-than-10? target)))))

    (testing "returns false when the provided value is nil"
      (let [target nil
            length-less-than-10? (preds/length-less-than? 10)]
        (is (false? (length-less-than-10? target)))))))

(deftest for-length-greater-than?
  (testing "returns a validator that"
    (testing
     "returns true when the provided string has length greater than specified"
      (let [target "abcdefghijk"
            length-greater-than-10? (preds/length-greater-than? 10)]
        (is (true? (length-greater-than-10? target)))))

    (testing "returns false when the provided string length equal to specified"
      (let [target "abcdefghij"
            length-greater-than-10? (preds/length-greater-than? 10)]
        (is (false? (length-greater-than-10? target)))))

    (testing
     "returns false when the provided string length less than specified"
      (let [target "abcdef"
            length-greater-than-10? (preds/length-greater-than? 10)]
        (is (false? (length-greater-than-10? target)))))

    (testing "returns false when the provided value is not a string"
      (let [target 25
            length-greater-than-10? (preds/length-greater-than? 10)]
        (is (false? (length-greater-than-10? target)))))

    (testing "returns false when the provided value is nil"
      (let [target nil
            length-greater-than-10? (preds/length-greater-than? 10)]
        (is (false? (length-greater-than-10? target)))))))

;; dates
(deftest for-iso8601-datetime?
  (testing "returns true when provided string is an ISO8601 datetime"
    (let [target "2019-01-01T12:00:00Z"]
      (is (true? (preds/iso8601-datetime? target)))))

  (testing "returns false when provided string is not an ISO8601 datetime"
    (let [target "the quick brown fox"]
      (is (false? (preds/iso8601-datetime? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 35]
      (is (false? (preds/iso8601-datetime? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (preds/iso8601-datetime? target))))))

;; currency
(deftest for-currency-amount?
  (testing "returns true when provided string represents a currency amount"
    (let [target "10.20"]
      (is (true? (preds/currency-amount? target)))))

  (testing
   "returns false when provided string does not represent a currency amount"
    (let [target "the quick brown fox"]
      (is (false? (preds/currency-amount? target)))))

  (testing "returns false when provided value is not a string"
    (let [target true]
      (is (false? (preds/currency-amount? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (preds/currency-amount? target))))))

(deftest for-currency-code?
  (testing "returns true when provided string represents a currency code"
    (let [target "JPY"]
      (is (true? (preds/currency-code? target)))))

  (testing
   "returns false when provided string does not represent a currency code"
    (let [target "the quick brown fox"]
      (is (false? (preds/currency-code? target)))))

  (testing "returns false when provided value is not a string"
    (let [target true]
      (is (false? (preds/currency-code? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (preds/currency-code? target))))))

;; postcodes
(deftest for-postcode?
  (testing "returns true when provided string represents a UK postcode"
    (let [target "EC1A 1BB"]
      (is (true? (preds/postcode? target)))))

  (testing "returns false when provided string does not represent a UK postcode"
    (let [target "the quick brown fox"]
      (is (false? (preds/postcode? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 35]
      (is (false? (preds/postcode? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (preds/postcode? target))))))

;; phone numbers
(deftest for-phone-number?
  (testing "returns true when provided string represents a phone number"
    (let [target "07890424158"]
      (is (true? (preds/phone-number? target)))))

  (testing
   "returns false when provided string does not represent a phone number"
    (let [target "the quick brown fox"]
      (is (false? (preds/phone-number? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 35]
      (is (false? (preds/phone-number? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (preds/phone-number? target))))))

;; email address
(deftest for-email-address?
  (testing "returns true when provided string is a valid email address"
    (let [target "person@example.com"]
      (is (true? (preds/email-address? target)))))

  (testing
   "returns false when provided string does not represent an email address"
    (let [target "the quick brown fox"]
      (is (false? (preds/email-address? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 35]
      (is (false? (preds/email-address? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (preds/email-address? target))))))
