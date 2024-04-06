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
   [spec.validate.utils :as sv-utils]

   [spec.validate.test-support.cases :as sv-cases]))

(defn string-sample [char-choices & {:keys [shuffle?]}]
  (let [chars
        (mapcat
          (fn [[chars rate]]
            (cond
              (= rate :once) [(rand-nth chars)]
              (> rate 1) (take rate (repeatedly #(rand-nth chars)))
              :else (random-sample rate chars)))
          char-choices)
        chars (if shuffle? (shuffle chars) chars)]
    (string/join chars)))

(deftest string?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "a string" :sample "stuff")
     (sv-cases/false-case "a non-string" :sample 35)
     (sv-cases/false-case "nil" :sample nil)]]
    (testing (str "for " (:title case))
      (is (every?
            #(= (sv-string/string? %) (:satisfied? case))
            (:samples case))))))

(deftest string?-as-requirement
  (is (= :must-be-a-string
        (sv-core/pred-requirement
          'spec.validate.string/string?)))
  (is (= :must-be-a-string
        (sv-core/pred-requirement
          'clojure.core/string?))))

(deftest string?-as-generator
  (is (every? true?
        (map #(sv-utils/re-satisfies? #"^\w*$" %)
          (gen/sample (spec/gen sv-string/string?))))))

(deftest blank?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any whitespace character"
       :samples sv-unicode/whitespace-characters)
     (sv-cases/true-case "a sequence of whitespace characters"
       :sample (string-sample [[sv-unicode/whitespace-characters 0.3]]))
     (sv-cases/true-case "an empty string" :sample "")
     (sv-cases/false-case "non-whitespace characters"
       :samples sv-unicode/non-whitespace-characters)
     (sv-cases/false-case "sequence of non-whitespace characters"
       :sample (string-sample
                 [[sv-unicode/non-whitespace-characters 0.00001]]))
     (sv-cases/false-case
       "a sequence containing both whitespace and non-whitespace characters"
       :sample (string-sample
                 [[sv-unicode/whitespace-characters 0.3]
                  [sv-unicode/non-whitespace-characters 0.00001]]
                 {:shuffle? true}))
     (sv-cases/false-case "a non string" :sample 10)
     (sv-cases/false-case "nil" :sample nil)]]
    (testing (str "for " (:title case))
      (is (every?
            #(= (sv-string/blank? %) (:satisfied? case))
            (:samples case))))))

(deftest blank?-as-requirement
  (is (= :must-be-a-blank-string
        (sv-core/pred-requirement
          'spec.validate.string/blank?))))

(deftest blank?-as-generator
  (testing "generates some empty strings"
    (let [samples (gen/sample (spec/gen sv-string/blank?) 100)]
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
                  sv-unicode/whitespace-character?
                  (sv-unicode/unicode-codepoint-seq sample)))
              samples))))))

(deftest not-blank?-as-predicate
  (doseq
   [case
    [(sv-cases/false-case "any whitespace character"
       :samples sv-unicode/whitespace-characters)
     (sv-cases/false-case "a sequence of whitespace characters"
       :sample (string-sample [[sv-unicode/whitespace-characters 0.3]]))
     (sv-cases/false-case "an empty string" :sample "")
     (sv-cases/true-case "any non-whitespace character"
       :samples sv-unicode/non-whitespace-characters)
     (sv-cases/true-case "a sequence of non-whitespace characters"
       :sample (string-sample
                 [[sv-unicode/non-whitespace-characters 0.00001]]))
     (sv-cases/true-case
       "a sequence containing both whitespace and non-whitespace characters"
       :sample (string-sample
                 [[sv-unicode/whitespace-characters 0.3]
                  [sv-unicode/non-whitespace-characters 0.00001]]
                 {:shuffle? true}))
     (sv-cases/false-case "a non-string" :sample 10)
     (sv-cases/false-case "nil" :sample nil)]]
    (testing (str "for " (:title case))
      (is (every?
            #(= (sv-string/not-blank? %) (:satisfied? case))
            (:samples case))))))

(deftest not-blank?-as-requirement
  (is (= :must-be-a-non-blank-string
        (sv-core/pred-requirement
          'spec.validate.string/not-blank?))))

(deftest not-blank?-as-generator
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
                  sv-unicode/non-whitespace-character?
                  (sv-unicode/unicode-codepoint-seq sample)))
              samples))))))

(deftest ascii-digits?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any ASCII digit character"
       :samples sv-unicode/ascii-digit-characters)
     (sv-cases/true-case "a sequence of ASCII digit characters"
       :sample (string-sample
                 [[sv-unicode/ascii-digit-characters :once]
                  [sv-unicode/ascii-digit-characters 0.3]]))
     (sv-cases/false-case "an empty string" :sample "")
     (sv-cases/false-case "any non ASCII digit character"
       :samples sv-unicode/non-ascii-digit-characters)
     (sv-cases/false-case "a sequence of non-ASCII digit characters"
       :sample (string-sample
                 [[sv-unicode/non-ascii-digit-characters 0.00001]]))
     (sv-cases/false-case
       "a sequence containing both ASCII digit and non-ASCII digit characters"
       :sample (string-sample
                 [[sv-unicode/ascii-digit-characters 0.3]
                  [sv-unicode/non-ascii-digit-characters 0.00001]]
                 {:shuffle? true}))
     (sv-cases/false-case "a non-string" :sample 10)
     (sv-cases/false-case "nil" :sample nil)]]
    (testing (str "for " (:title case))
      (is (every?
            #(= (sv-string/ascii-digits? %) (:satisfied? case))
            (:samples case))))))

(deftest ascii-digits?-as-requirement
  (is (= :must-be-a-string-of-ascii-digits
        (sv-core/pred-requirement
          'spec.validate.string/ascii-digits?))))

(deftest ascii-digits?-as-generator
  (testing "generates no empty strings"
    (is (empty?
          (filter
            (fn [sample] (= 0 (count sample)))
            (gen/sample (spec/gen sv-string/ascii-digits?) 100)))))

  (testing "generates only ASCII digit strings"
    (is (every? true?
          (map #(sv-utils/re-satisfies? #"^\d+$" %)
            (gen/sample (spec/gen sv-string/ascii-digits?) 100))))))

(deftest lowercase-ascii-alphabetics?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any lowercase ASCII alphabetic character"
       :samples sv-unicode/lowercase-ascii-alphabetic-characters)
     (sv-cases/true-case "a sequence of lowercase ASCII alphabetic characters"
       :sample (string-sample
                 [[sv-unicode/lowercase-ascii-alphabetic-characters :once]
                  [sv-unicode/lowercase-ascii-alphabetic-characters 0.3]]))
     (sv-cases/false-case "an empty string" :sample "")
     (sv-cases/false-case "any non non lowercase ASCII alphabetic character"
       :samples sv-unicode/non-lowercase-ascii-alphabetic-characters)
     (sv-cases/false-case
       "a sequence of non lowercase ASCII alphabetic characters"
       :sample
       (string-sample
         [[sv-unicode/non-lowercase-ascii-alphabetic-characters 0.00001]]))
     (sv-cases/false-case
       (str "a sequence containing both lowercase ASCII alphabetic "
         "characters and non lowercase ASCII alphabetic characters")
       :sample
       (string-sample
         [[sv-unicode/lowercase-ascii-alphabetic-characters 0.3]
          [sv-unicode/non-lowercase-ascii-alphabetic-characters 0.00001]]
         {:shuffle? true}))
     (sv-cases/false-case "a non-string" :sample 10)
     (sv-cases/false-case "nil" :sample nil)]]
    (testing (str "for " (:title case))
      (is (every?
            #(= (sv-string/lowercase-ascii-alphabetics? %) (:satisfied? case))
            (:samples case))))))

(deftest lowercase-ascii-alphabetics?-as-requirement
  (is (= :must-be-a-string-of-lowercase-ascii-alphabetic-characters
        (sv-core/pred-requirement
          'spec.validate.string/lowercase-ascii-alphabetics?))))

(deftest lowercase-ascii-alphabetics?-as-generator
  (testing "generates no empty strings"
    (is (empty?
          (filter
            (fn [sample] (= 0 (count sample)))
            (gen/sample
              (spec/gen sv-string/lowercase-ascii-alphabetics?) 100)))))

  (testing "generates only lowercase ASCII alphabetic strings"
    (is (every? true?
          (map #(sv-utils/re-satisfies? #"^[a-z]+$" %)
            (gen/sample
              (spec/gen sv-string/lowercase-ascii-alphabetics?) 100))))))

(deftest uppercase-ascii-alphabetics?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any uppercase ASCII alphabetic character"
       :samples sv-unicode/uppercase-ascii-alphabetic-characters)
     (sv-cases/true-case "a sequence of uppercase ASCII alphabetic characters"
       :sample (string-sample
                 [[sv-unicode/uppercase-ascii-alphabetic-characters :once]
                  [sv-unicode/uppercase-ascii-alphabetic-characters 0.3]]))
     (sv-cases/false-case "an empty string" :sample "")
     (sv-cases/false-case "any non non uppercase ASCII alphabetic character"
       :samples sv-unicode/non-uppercase-ascii-alphabetic-characters)
     (sv-cases/false-case
       "a sequence of non uppercase ASCII alphabetic characters"
       :sample
       (string-sample
         [[sv-unicode/non-uppercase-ascii-alphabetic-characters 0.00001]]))
     (sv-cases/false-case
       (str "a sequence containing both uppercase ASCII alphabetic "
         "characters and non uppercase ASCII alphabetic characters")
       :sample
       (string-sample
         [[sv-unicode/uppercase-ascii-alphabetic-characters 0.3]
          [sv-unicode/non-uppercase-ascii-alphabetic-characters 0.00001]]
         {:shuffle? true}))
     (sv-cases/false-case "a non-string" :sample 10)
     (sv-cases/false-case "nil" :sample nil)]]
    (testing (str "for " (:title case))
      (is (every?
            #(= (sv-string/uppercase-ascii-alphabetics? %) (:satisfied? case))
            (:samples case))))))

(deftest uppercase-ascii-alphabetics?-as-requirement
  (is (= :must-be-a-string-of-uppercase-ascii-alphabetic-characters
        (sv-core/pred-requirement
          'spec.validate.string/uppercase-ascii-alphabetics?))))

(deftest uppercase-ascii-alphabetics?-as-generator
  (testing "generates no empty strings"
    (is (empty?
          (filter
            (fn [sample] (= 0 (count sample)))
            (gen/sample
              (spec/gen sv-string/uppercase-ascii-alphabetics?) 100)))))

  (testing "generates only uppercase ASCII alphabetic strings"
    (is (every? true?
          (map #(sv-utils/re-satisfies? #"^[A-Z]+$" %)
            (gen/sample
              (spec/gen sv-string/uppercase-ascii-alphabetics?) 100))))))

(deftest ascii-alphabetics?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any ASCII alphabetic character"
       :samples sv-unicode/ascii-alphabetic-characters)
     (sv-cases/true-case "a sequence of ASCII alphabetic characters"
       :sample (string-sample
                 [[sv-unicode/ascii-alphabetic-characters :once]
                  [sv-unicode/ascii-alphabetic-characters 0.3]]))
     (sv-cases/false-case "an empty string" :sample "")
     (sv-cases/false-case "any non ASCII alphabetic character"
       :samples sv-unicode/non-ascii-alphabetic-characters)
     (sv-cases/false-case "a sequence of non ASCII alphabetic characters"
       :sample (string-sample
                 [[sv-unicode/non-ascii-alphabetic-characters 0.00001]]))
     (sv-cases/false-case
       (str "a sequence containing both ASCII alphabetic characters and non "
         "ASCII alphabetic characters")
       :sample
       (string-sample
         [[sv-unicode/ascii-alphabetic-characters 0.3]
          [sv-unicode/non-ascii-alphabetic-characters 0.00001]]
         {:shuffle? true}))
     (sv-cases/false-case "a non-string" :sample 10)
     (sv-cases/false-case "nil" :sample nil)]]
    (testing (str "for " (:title case))
      (is (every?
            #(= (sv-string/ascii-alphabetics? %) (:satisfied? case))
            (:samples case))))))

(deftest ascii-alphabetics?-as-requirement
  (is (= :must-be-a-string-of-ascii-alphabetic-characters
        (sv-core/pred-requirement
          'spec.validate.string/ascii-alphabetics?))))

(deftest ascii-alphabetics?-as-generator
  (testing "generates no empty strings"
    (is (empty?
          (filter
            (fn [sample] (= 0 (count sample)))
            (gen/sample (spec/gen sv-string/ascii-alphabetics?) 100)))))

  (testing "generates only ASCII alphabetic strings"
    (is (every? true?
          (map #(sv-utils/re-satisfies? #"^[a-zA-Z]+$" %)
            (gen/sample (spec/gen sv-string/ascii-alphabetics?) 100))))))

(deftest lowercase-ascii-alphanumerics?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any lowercase ASCII alphanumeric character"
       :samples sv-unicode/lowercase-ascii-alphanumeric-characters)
     (sv-cases/true-case
       "a sequence of lowercase ASCII alphanumeric characters"
       :sample (string-sample
                 [[sv-unicode/lowercase-ascii-alphanumeric-characters :once]
                  [sv-unicode/lowercase-ascii-alphanumeric-characters 0.3]]))
     (sv-cases/false-case "an empty string" :sample "")
     (sv-cases/false-case "any non lowercase ASCII alphanumeric character"
       :samples sv-unicode/non-lowercase-ascii-alphanumeric-characters)
     (sv-cases/false-case
       "a sequence of non lowercase ASCII alphanumeric characters"
       :sample
       (string-sample
         [[sv-unicode/non-lowercase-ascii-alphanumeric-characters 0.00001]]))
     (sv-cases/false-case
       (str "a sequence containing both lowercase ASCII alphanumeric "
         "characters and non ASCII alphanumeric characters")
       :sample
       (string-sample
         [[sv-unicode/lowercase-ascii-alphanumeric-characters 0.3]
          [sv-unicode/non-lowercase-ascii-alphanumeric-characters 0.00001]]
         {:shuffle? true}))
     (sv-cases/false-case "a non-string" :sample 10)
     (sv-cases/false-case "nil" :sample nil)]]
    (testing (str "for " (:title case))
      (is (every?
            #(= (sv-string/lowercase-ascii-alphanumerics? %) (:satisfied? case))
            (:samples case))))))

(deftest lowercase-ascii-alphanumerics?-as-requirement
  (is (= :must-be-a-string-of-lowercase-ascii-alphanumeric-characters
        (sv-core/pred-requirement
          'spec.validate.string/lowercase-ascii-alphanumerics?))))

(deftest lowercase-ascii-alphanumerics?-as-generator
  (testing "generates no empty strings"
    (is (empty?
          (filter
            (fn [sample] (= 0 (count sample)))
            (gen/sample
              (spec/gen sv-string/lowercase-ascii-alphanumerics?) 100)))))

  (testing "generates only lowercase ASCII alphanumeric strings"
    (is (every? true?
          (map #(sv-utils/re-satisfies? #"^[a-z0-9]+$" %)
            (gen/sample
              (spec/gen sv-string/lowercase-ascii-alphanumerics?) 100))))))

(deftest uppercase-ascii-alphanumerics?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any uppercase ASCII alphanumeric character"
       :samples sv-unicode/uppercase-ascii-alphanumeric-characters)
     (sv-cases/true-case
       "a sequence of uppercase ASCII alphanumeric characters"
       :sample (string-sample
                 [[sv-unicode/uppercase-ascii-alphanumeric-characters :once]
                  [sv-unicode/uppercase-ascii-alphanumeric-characters 0.3]]))
     (sv-cases/false-case "an empty string" :sample "")
     (sv-cases/false-case "any non uppercase ASCII alphanumeric character"
       :samples sv-unicode/non-uppercase-ascii-alphanumeric-characters)
     (sv-cases/false-case
       "a sequence of non lowercase ASCII alphanumeric characters"
       :sample
       (string-sample
         [[sv-unicode/non-uppercase-ascii-alphanumeric-characters 0.00001]]))
     (sv-cases/false-case
       (str "a sequence containing both uppercase ASCII alphanumeric "
         "characters and non ASCII alphanumeric characters")
       :sample
       (string-sample
         [[sv-unicode/uppercase-ascii-alphanumeric-characters 0.3]
          [sv-unicode/non-uppercase-ascii-alphanumeric-characters 0.00001]]
         {:shuffle? true}))
     (sv-cases/false-case "a non-string" :sample 10)
     (sv-cases/false-case "nil" :sample nil)]]
    (testing (str "for " (:title case))
      (is (every?
            #(= (sv-string/uppercase-ascii-alphanumerics? %) (:satisfied? case))
            (:samples case))))))

(deftest uppercase-ascii-alphanumerics?-as-requirement
  (is (= :must-be-a-string-of-uppercase-ascii-alphanumeric-characters
        (sv-core/pred-requirement
          'spec.validate.string/uppercase-ascii-alphanumerics?))))

(deftest uppercase-ascii-alphanumerics?-as-generator
  (testing "generates no empty strings"
    (is (empty?
          (filter
            (fn [sample] (= 0 (count sample)))
            (gen/sample
              (spec/gen sv-string/uppercase-ascii-alphanumerics?) 100)))))

  (testing "generates only uppercase ASCII alphanumeric strings"
    (is (every? true?
          (map #(sv-utils/re-satisfies? #"^[A-Z0-9]+$" %)
            (gen/sample
              (spec/gen sv-string/uppercase-ascii-alphanumerics?) 100))))))

(deftest ascii-alphanumerics?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any ASCII alphanumeric character"
       :samples sv-unicode/ascii-alphanumeric-characters)
     (sv-cases/true-case
       "a sequence of uppercase ASCII alphanumeric characters"
       :sample (string-sample
                 [[sv-unicode/ascii-alphanumeric-characters :once]
                  [sv-unicode/ascii-alphanumeric-characters 0.3]]))
     (sv-cases/false-case "an empty string" :sample "")
     (sv-cases/false-case "any non uppercase ASCII alphanumeric character"
       :samples sv-unicode/non-ascii-alphanumeric-characters)
     (sv-cases/false-case
       "a sequence of non lowercase ASCII alphanumeric characters"
       :sample
       (string-sample
         [[sv-unicode/non-ascii-alphanumeric-characters 0.00001]]))
     (sv-cases/false-case
       (str "a sequence containing both uppercase ASCII alphanumeric "
         "characters and non ASCII alphanumeric characters")
       :sample
       (string-sample
         [[sv-unicode/ascii-alphanumeric-characters 0.3]
          [sv-unicode/non-ascii-alphanumeric-characters 0.00001]]
         {:shuffle? true}))
     (sv-cases/false-case "a non-string" :sample 10)
     (sv-cases/false-case "nil" :sample nil)]]
    (testing (str "for " (:title case))
      (is (every?
            #(= (sv-string/ascii-alphanumerics? %) (:satisfied? case))
            (:samples case))))))

(deftest ascii-alphanumerics?-as-requirement
  (is (= :must-be-a-string-of-ascii-alphanumeric-characters
        (sv-core/pred-requirement
          'spec.validate.string/ascii-alphanumerics?))))

(deftest ascii-alphanumerics?-as-generator
  (testing "generates no empty strings"
    (is (empty?
          (filter
            (fn [sample] (= 0 (count sample)))
            (gen/sample
              (spec/gen sv-string/ascii-alphanumerics?) 100)))))

  (testing "generates only ASCII alphabetic strings"
    (is (every? true?
          (map #(sv-utils/re-satisfies? #"^[a-zA-Z0-9]+$" %)
            (gen/sample
              (spec/gen sv-string/ascii-alphanumerics?) 100))))))
