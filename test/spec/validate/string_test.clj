(ns spec.validate.string-test
  (:refer-clojure :exclude [string?])
  (:require
    [clojure.test :refer [deftest is testing]]
    [clojure.string :as string]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.alpha :as spec]

    [spec.validate.core :as sv-core]
    [spec.validate.string :as sv-string]
    [spec.validate.unicode :as sv-unicode]

    [spec.validate.test-support.string :as sv-test-string]))

(defn re-satisfies? [re s]
  (not (nil? (re-find re s))))

(deftest string?
  (testing "predicate"
    (is (true? (sv-string/string? "stuff")))
    (is (false? (sv-string/string? 35)))
    (is (false? (sv-string/string? nil))))

  (testing "requirement"
    (is (= :must-be-a-string
          (sv-core/pred-requirement
            'spec.validate.string/string?)))
    (is (= :must-be-a-string
          (sv-core/pred-requirement
            'clojure.core/string?))))

  (testing "generation"
    (is (every? true?
          (map #(re-satisfies? #"^\w*$" %)
            (gen/sample (spec/gen sv-string/string?)))))))

(deftest blank?
  (testing "predicate"
    (testing "for any whitespace character"
      (is (every?
            #(sv-string/blank? %)
            sv-unicode/whitespace-characters)))

    (testing "for a sequence of whitespace characters"
      (let [whitespace-string
            (string/join
              (random-sample 0.3 sv-unicode/whitespace-characters))]
        (is (true? (sv-string/blank? whitespace-string)))))

    (testing "for an empty string"
      (is (true? (sv-string/blank? ""))))

    (testing "for non-whitespace characters"
      (is (not-any?
            #(sv-string/blank? %)
            sv-unicode/non-whitespace-characters)))

    (testing "for a sequence of non-whitespace characters"
      (let [non-whitespace-string
            (string/join
              (random-sample 0.00001 sv-unicode/non-whitespace-characters))]
        (is (false? (sv-string/blank? non-whitespace-string)))))

    (testing
      "for a sequence containing both whitespace and non-whitespace characters"
      (let [mixed-string
            (string/join
              (shuffle
                (concat
                  (random-sample 0.3
                    sv-unicode/whitespace-characters)
                  (random-sample 0.00001
                    sv-unicode/non-whitespace-characters))))]
        (is (false? (sv-string/blank? mixed-string)))))

    (testing "for a non-string"
      (is (false? (sv-string/blank? 10))))

    (testing "for nil"
      (is (false? (sv-string/blank? nil)))))

  (testing "requirement"
    (is (= :must-be-a-blank-string
          (sv-core/pred-requirement
            'spec.validate.string/blank?))))

  (testing "generation"
    (testing "generates some empty strings"
      (let [samples (gen/sample (spec/gen sv-string/blank?) 100)]
        (clojure.pprint/pprint samples)
        (is (not (empty?
                   (filter
                     (fn [sample] (= 0 (count sample)))
                     samples))))))

    (testing "generates only whitespace strings"
      (let [samples (gen/sample (spec/gen sv-string/blank?) 100)]
        (is (every? true?
              (map
                (fn [sample]
                  (every?
                    sv-test-string/whitespace?
                    (sv-unicode/unicode-codepoint-seq sample)))
                samples)))))))

(deftest not-blank?
  (testing "predicate"
    (testing "for any whitespace character"
      (is (not-any?
            #(sv-string/not-blank? %)
            sv-unicode/whitespace-characters)))

    (testing "for a sequence of whitespace characters"
      (let [whitespace-string
            (string/join
              (random-sample 0.3 sv-unicode/whitespace-characters))]
        (is (false? (sv-string/not-blank? whitespace-string)))))

    (testing "for an empty string"
      (is (false? (sv-string/not-blank? ""))))

    (testing "for any non-whitespace character"
      (is (every?
            #(sv-string/not-blank? %)
            sv-unicode/non-whitespace-characters)))

    (testing "for a sequence of non-whitespace characters"
      (let [non-whitespace-string
            (string/join
              (random-sample 0.00001 sv-unicode/non-whitespace-characters))]
        (is (true? (sv-string/not-blank? non-whitespace-string)))))

    (testing
      "for a sequence containing both whitespace and non-whitespace characters"
      (let [mixed-string
            (string/join
              (shuffle
                (concat
                  (random-sample 0.3
                    sv-unicode/whitespace-characters)
                  (random-sample 0.00001
                    sv-unicode/non-whitespace-characters))))]
        (is (true? (sv-string/not-blank? mixed-string)))))

    (testing "for a non-string"
      (is (false? (sv-string/not-blank? 10))))

    (testing "for nil"
      (is (false? (sv-string/not-blank? nil)))))

  (testing "requirement"
    (is (= :must-be-a-non-blank-string
          (sv-core/pred-requirement
            'spec.validate.string/not-blank?))))

  (testing "generation"
    (testing "generates no empty strings"
      (let [samples (gen/sample (spec/gen sv-string/not-blank?) 100)]
        (is (empty?
              (filter
                (fn [sample] (= 0 (count sample)))
                samples)))))

    (testing "generates only non-whitespace strings"
      (let [samples (gen/sample (spec/gen sv-string/not-blank?) 100)]
        (is (every? true?
              (map
                (fn [sample]
                  (some
                    sv-test-string/non-whitespace?
                    (sv-unicode/unicode-codepoint-seq sample)))
                samples)))))))
