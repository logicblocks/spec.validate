(ns spec.validate.url-test
  (:require
   [clojure.test :refer [deftest is testing]]

   [spec.validate.url :as sv-url]))

(deftest for-absolute-url?
  (testing "returns true when provided string represents an absolute URL"
    (let [target "https://example.com/some/path/to/file.html"]
      (is (true? (sv-url/absolute-url? target)))))

  (testing "returns false when provided string is not absolute"
    (let [target "/some/path/to/file.html"]
      (is (false? (sv-url/absolute-url? target)))))

  (testing "returns false when provided string is not a url"
    (let [target "the quick brown fox"]
      (is (false? (sv-url/absolute-url? target)))))

  (testing "returns false when provided value is not a string"
    (let [target 35]
      (is (false? (sv-url/absolute-url? target)))))

  (testing "returns false when provided value is nil"
    (let [target nil]
      (is (false? (sv-url/absolute-url? target))))))
