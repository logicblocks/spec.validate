(ns spec.validate.uri-test
  (:require
   [clojure.test :refer [deftest is testing]]

   [spec.validate.uri :as sv-uri]))

(deftest for-absolute-url?
  (testing "returns true when provided string represents an absolute URL"
    (let [target "https://example.com/some/path/to/file.html"]
      (is (true? (sv-uri/absolute-url? target)))))

  (testing "returns false when provided string is not absolute"
    (let [target "/some/path/to/file.html"]
      (is (false? (sv-uri/absolute-url? target)))))

  (testing "returns false when provided string is not a url"
    (let [target "the quick brown fox"]
      (is (false? (sv-uri/absolute-url? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 35]
      (is (false? (sv-uri/absolute-url? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (sv-uri/absolute-url? target))))))
