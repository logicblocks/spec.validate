(ns datatype.string.core-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest]]

   [icu4clj.text.unicode-set :as icu-tus]
   [icu4clj.text.unicode-set-patterns :as icu-tusp]

   [datatype.string.core :as dt-string]

   [spec.test-support.cases :as sv-cases]))

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

(declare
  whitespace-characters
  non-whitespace-characters
  ascii-digit-characters
  non-ascii-digit-characters
  lowercase-ascii-alphabetic-characters
  non-lowercase-ascii-alphabetic-characters
  uppercase-ascii-alphabetic-characters
  non-uppercase-ascii-alphabetic-characters
  ascii-alphabetic-characters
  non-ascii-alphabetic-characters
  lowercase-ascii-alphanumeric-characters
  non-lowercase-ascii-alphanumeric-characters
  uppercase-ascii-alphanumeric-characters
  non-uppercase-ascii-alphanumeric-characters
  ascii-alphanumeric-characters
  non-ascii-alphanumeric-characters)

(defmacro def-chars [name pattern]
  `(def ~name
     (icu-tus/character-vector ~pattern)))

(def-chars whitespace-characters
  icu-tusp/whitespace-pattern)
(def-chars non-whitespace-characters
  icu-tusp/non-whitespace-pattern)
(def-chars ascii-digit-characters
  icu-tusp/ascii-digit-pattern)
(def-chars non-ascii-digit-characters
  icu-tusp/non-ascii-digit-pattern)
(def-chars lowercase-ascii-alphabetic-characters
  icu-tusp/lowercase-ascii-alphabetic-pattern)
(def-chars non-lowercase-ascii-alphabetic-characters
  icu-tusp/non-lowercase-ascii-alphabetic-pattern)
(def-chars uppercase-ascii-alphabetic-characters
  icu-tusp/uppercase-ascii-alphabetic-pattern)
(def-chars non-uppercase-ascii-alphabetic-characters
  icu-tusp/non-uppercase-ascii-alphabetic-pattern)
(def-chars ascii-alphabetic-characters
  icu-tusp/ascii-alphabetic-pattern)
(def-chars non-ascii-alphabetic-characters
  icu-tusp/non-ascii-alphabetic-pattern)
(def-chars lowercase-ascii-alphanumeric-characters
  icu-tusp/lowercase-ascii-alphanumeric-pattern)
(def-chars non-lowercase-ascii-alphanumeric-characters
  icu-tusp/non-lowercase-ascii-alphanumeric-pattern)
(def-chars uppercase-ascii-alphanumeric-characters
  icu-tusp/uppercase-ascii-alphanumeric-pattern)
(def-chars non-uppercase-ascii-alphanumeric-characters
  icu-tusp/non-uppercase-ascii-alphanumeric-pattern)
(def-chars ascii-alphanumeric-characters
  icu-tusp/ascii-alphanumeric-pattern)
(def-chars non-ascii-alphanumeric-characters
  icu-tusp/non-ascii-alphanumeric-pattern)

(deftest blank?
  (sv-cases/assert-cases-satisfied-by dt-string/blank?
    (sv-cases/true-case "any whitespace character"
      :samples whitespace-characters)
    (sv-cases/true-case "a sequence of whitespace characters"
      :sample (string-sample [[whitespace-characters 0.3]]))
    (sv-cases/true-case "an empty string" :sample "")
    (sv-cases/false-case "non-whitespace characters"
      :samples non-whitespace-characters)
    (sv-cases/false-case "sequence of non-whitespace characters"
      :sample (string-sample [[non-whitespace-characters 0.00001]]))
    (sv-cases/false-case
      "a sequence containing both whitespace and non-whitespace characters"
      :sample (string-sample
                [[whitespace-characters 0.3]
                 [non-whitespace-characters 0.00001]]
                {:shuffle? true}))
    (sv-cases/false-case "a non string" :sample 10)
    (sv-cases/false-case "nil" :sample nil)))

(deftest not-blank?
  (sv-cases/assert-cases-satisfied-by dt-string/not-blank?
    (sv-cases/false-case "any whitespace character"
      :samples whitespace-characters)
    (sv-cases/false-case "a sequence of whitespace characters"
      :sample (string-sample [[whitespace-characters 0.3]]))
    (sv-cases/false-case "an empty string" :sample "")
    (sv-cases/true-case "any non-whitespace character"
      :samples non-whitespace-characters)
    (sv-cases/true-case "a sequence of non-whitespace characters"
      :sample (string-sample [[non-whitespace-characters 0.00001]]))
    (sv-cases/true-case
      "a sequence containing both whitespace and non-whitespace characters"
      :sample (string-sample
                [[whitespace-characters 0.3]
                 [non-whitespace-characters 0.00001]]
                {:shuffle? true}))
    (sv-cases/false-case "a non-string" :sample 10)
    (sv-cases/false-case "nil" :sample nil)))

(deftest ascii-digits?
  (sv-cases/assert-cases-satisfied-by dt-string/ascii-digits?
    (sv-cases/true-case "any ASCII digit character"
      :samples ascii-digit-characters)
    (sv-cases/true-case "a sequence of ASCII digit characters"
      :sample (string-sample
                [[ascii-digit-characters :once]
                 [ascii-digit-characters 0.3]]))
    (sv-cases/false-case "an empty string" :sample "")
    (sv-cases/false-case "any non ASCII digit character"
      :samples non-ascii-digit-characters)
    (sv-cases/false-case "a sequence of non-ASCII digit characters"
      :sample (string-sample
                [[non-ascii-digit-characters 0.00001]]))
    (sv-cases/false-case
      "a sequence containing both ASCII digit and non-ASCII digit characters"
      :sample (string-sample
                [[ascii-digit-characters 0.3]
                 [non-ascii-digit-characters 0.00001]]
                {:shuffle? true}))
    (sv-cases/false-case "a non-string" :sample 10)
    (sv-cases/false-case "nil" :sample nil)))

(deftest lowercase-ascii-alphabetics?
  (sv-cases/assert-cases-satisfied-by dt-string/lowercase-ascii-alphabetics?
    (sv-cases/true-case "any lowercase ASCII alphabetic character"
      :samples lowercase-ascii-alphabetic-characters)
    (sv-cases/true-case "a sequence of lowercase ASCII alphabetic characters"
      :sample (string-sample
                [[lowercase-ascii-alphabetic-characters :once]
                 [lowercase-ascii-alphabetic-characters 0.3]]))
    (sv-cases/false-case "an empty string" :sample "")
    (sv-cases/false-case "any non non lowercase ASCII alphabetic character"
      :samples non-lowercase-ascii-alphabetic-characters)
    (sv-cases/false-case
      "a sequence of non lowercase ASCII alphabetic characters"
      :sample
      (string-sample
        [[non-lowercase-ascii-alphabetic-characters 0.00001]]))
    (sv-cases/false-case
      (str "a sequence containing both lowercase ASCII alphabetic "
        "characters and non lowercase ASCII alphabetic characters")
      :sample
      (string-sample
        [[lowercase-ascii-alphabetic-characters 0.3]
         [non-lowercase-ascii-alphabetic-characters 0.00001]]
        {:shuffle? true}))
    (sv-cases/false-case "a non-string" :sample 10)
    (sv-cases/false-case "nil" :sample nil)))

(deftest uppercase-ascii-alphabetics?
  (sv-cases/assert-cases-satisfied-by dt-string/uppercase-ascii-alphabetics?
    (sv-cases/true-case "any uppercase ASCII alphabetic character"
      :samples uppercase-ascii-alphabetic-characters)
    (sv-cases/true-case "a sequence of uppercase ASCII alphabetic characters"
      :sample (string-sample
                [[uppercase-ascii-alphabetic-characters :once]
                 [uppercase-ascii-alphabetic-characters 0.3]]))
    (sv-cases/false-case "an empty string" :sample "")
    (sv-cases/false-case "any non non uppercase ASCII alphabetic character"
      :samples non-uppercase-ascii-alphabetic-characters)
    (sv-cases/false-case
      "a sequence of non uppercase ASCII alphabetic characters"
      :sample
      (string-sample
        [[non-uppercase-ascii-alphabetic-characters 0.00001]]))
    (sv-cases/false-case
      (str "a sequence containing both uppercase ASCII alphabetic "
        "characters and non uppercase ASCII alphabetic characters")
      :sample
      (string-sample
        [[uppercase-ascii-alphabetic-characters 0.3]
         [non-uppercase-ascii-alphabetic-characters 0.00001]]
        {:shuffle? true}))
    (sv-cases/false-case "a non-string" :sample 10)
    (sv-cases/false-case "nil" :sample nil)))

(deftest ascii-alphabetics?
  (sv-cases/assert-cases-satisfied-by dt-string/ascii-alphabetics?
    (sv-cases/true-case "any ASCII alphabetic character"
      :samples ascii-alphabetic-characters)
    (sv-cases/true-case "a sequence of ASCII alphabetic characters"
      :sample (string-sample
                [[ascii-alphabetic-characters :once]
                 [ascii-alphabetic-characters 0.3]]))
    (sv-cases/false-case "an empty string" :sample "")
    (sv-cases/false-case "any non ASCII alphabetic character"
      :samples non-ascii-alphabetic-characters)
    (sv-cases/false-case "a sequence of non ASCII alphabetic characters"
      :sample (string-sample
                [[non-ascii-alphabetic-characters 0.00001]]))
    (sv-cases/false-case
      (str "a sequence containing both ASCII alphabetic characters and non "
        "ASCII alphabetic characters")
      :sample
      (string-sample
        [[ascii-alphabetic-characters 0.3]
         [non-ascii-alphabetic-characters 0.00001]]
        {:shuffle? true}))
    (sv-cases/false-case "a non-string" :sample 10)
    (sv-cases/false-case "nil" :sample nil)))

(deftest lowercase-ascii-alphanumerics?
  (sv-cases/assert-cases-satisfied-by dt-string/lowercase-ascii-alphanumerics?
    (sv-cases/true-case "any lowercase ASCII alphanumeric character"
      :samples lowercase-ascii-alphanumeric-characters)
    (sv-cases/true-case
      "a sequence of lowercase ASCII alphanumeric characters"
      :sample (string-sample
                [[lowercase-ascii-alphanumeric-characters :once]
                 [lowercase-ascii-alphanumeric-characters 0.3]]))
    (sv-cases/false-case "an empty string" :sample "")
    (sv-cases/false-case "any non lowercase ASCII alphanumeric character"
      :samples non-lowercase-ascii-alphanumeric-characters)
    (sv-cases/false-case
      "a sequence of non lowercase ASCII alphanumeric characters"
      :sample
      (string-sample
        [[non-lowercase-ascii-alphanumeric-characters 0.00001]]))
    (sv-cases/false-case
      (str "a sequence containing both lowercase ASCII alphanumeric "
        "characters and non ASCII alphanumeric characters")
      :sample
      (string-sample
        [[lowercase-ascii-alphanumeric-characters 0.3]
         [non-lowercase-ascii-alphanumeric-characters 0.00001]]
        {:shuffle? true}))
    (sv-cases/false-case "a non-string" :sample 10)
    (sv-cases/false-case "nil" :sample nil)))

(deftest uppercase-ascii-alphanumerics?
  (sv-cases/assert-cases-satisfied-by dt-string/uppercase-ascii-alphanumerics?
    (sv-cases/true-case "any uppercase ASCII alphanumeric character"
      :samples uppercase-ascii-alphanumeric-characters)
    (sv-cases/true-case
      "a sequence of uppercase ASCII alphanumeric characters"
      :sample (string-sample
                [[uppercase-ascii-alphanumeric-characters :once]
                 [uppercase-ascii-alphanumeric-characters 0.3]]))
    (sv-cases/false-case "an empty string" :sample "")
    (sv-cases/false-case "any non uppercase ASCII alphanumeric character"
      :samples non-uppercase-ascii-alphanumeric-characters)
    (sv-cases/false-case
      "a sequence of non lowercase ASCII alphanumeric characters"
      :sample
      (string-sample
        [[non-uppercase-ascii-alphanumeric-characters 0.00001]]))
    (sv-cases/false-case
      (str "a sequence containing both uppercase ASCII alphanumeric "
        "characters and non ASCII alphanumeric characters")
      :sample
      (string-sample
        [[uppercase-ascii-alphanumeric-characters 0.3]
         [non-uppercase-ascii-alphanumeric-characters 0.00001]]
        {:shuffle? true}))
    (sv-cases/false-case "a non-string" :sample 10)
    (sv-cases/false-case "nil" :sample nil)))

(deftest ascii-alphanumerics?
  (sv-cases/assert-cases-satisfied-by dt-string/ascii-alphanumerics?
    (sv-cases/true-case "any ASCII alphanumeric character"
      :samples ascii-alphanumeric-characters)
    (sv-cases/true-case
      "a sequence of uppercase ASCII alphanumeric characters"
      :sample (string-sample
                [[ascii-alphanumeric-characters :once]
                 [ascii-alphanumeric-characters 0.3]]))
    (sv-cases/false-case "an empty string" :sample "")
    (sv-cases/false-case "any non uppercase ASCII alphanumeric character"
      :samples non-ascii-alphanumeric-characters)
    (sv-cases/false-case
      "a sequence of non lowercase ASCII alphanumeric characters"
      :sample
      (string-sample
        [[non-ascii-alphanumeric-characters 0.00001]]))
    (sv-cases/false-case
      (str "a sequence containing both uppercase ASCII alphanumeric "
        "characters and non ASCII alphanumeric characters")
      :sample
      (string-sample
        [[ascii-alphanumeric-characters 0.3]
         [non-ascii-alphanumeric-characters 0.00001]]
        {:shuffle? true}))
    (sv-cases/false-case "a non-string" :sample 10)
    (sv-cases/false-case "nil" :sample nil)))
