(ns spec.validate.core-test
  (:refer-clojure :exclude [string? number?])
  (:require
   [clojure.test :refer :all]
   [clojure.spec.alpha :as spec]

   [spec.validate.core :as sv-core]))

(defn string?
  [value]
  (clojure.core/string? value))

(defmethod sv-core/pred-requirement
  'spec.validate.core-test/string?
  [_]
  :must-be-a-string)

(defn length-greater-than-5?
  [value]
  (> (count value) 5))

(defmethod sv-core/pred-requirement
  'spec.validate.core-test/length-greater-than-5?
  [_]
  :must-have-length-greater-than-5)

(def number?
  ^{:spec.validate/requirement :must-be-a-number}
  (fn [value] (clojure.core/number? value)))

(defmethod sv-core/pred-requirement
  'spec.validate.core-test/number?
  [_]
  :must-be-a-number)

(defn greater-than-5? [value]
  (> value 5))

(spec/def ::some-string string?)
(spec/def ::some-number number?)
(spec/def ::some-large-value greater-than-5?)

(spec/def ::some-complex-thing (spec/and string? length-greater-than-5?))

(spec/def ::other-string string?)
(spec/def ::other-number number?)

(spec/def ::some-arbitrary-constraint #(> % 5))
(spec/def ::some-set-member #{:club :diamond :heart :spade})
(spec/def ::some-union (spec/or :name string? :id number?))
(spec/def ::some-alternative (spec/alt :name string? :id number?))
(spec/def ::some-concatenation (spec/cat :a string? :b number?))
(spec/def ::some-nilable-thing (spec/nilable string?))
(spec/def ::some-collection
  (spec/coll-of keyword? :kind vector? :count 3 :distinct true :into #{}))
(spec/def ::some-tuple (spec/tuple string? number?))

(def spec ::some-alternative)
(spec/conform spec ["hi" 1])
(spec/explain-data spec [:yo])

(spec/def ::some-object
  (spec/keys
    :req-un [::some-string ::some-number ::some-large-value]))

(spec/def ::some-complex-object
  (spec/keys
    :req-un [::some-complex-thing]))

(spec/def ::other-object
  (spec/keys
    :req-un [::other-string ::other-number]))

(spec/def ::higher-order-object
  (spec/keys
    :req-un [::some-object ::other-object]))

(deftest about-validator-for
  (testing "when there are no problems"
    (let [target {:some-string      "correct"
                  :some-number      50
                  :some-large-value 10}
          valid? (sv-core/validator ::some-object)]
      (is (true? (valid? target)))))

  (testing "when there are problems"
    (let [target {:wrong-string "nope"}
          valid? (sv-core/validator ::some-object)]
      (is (false? (valid? target))))))

(deftest about-problem-calculator-for
  (testing "when there are no problems"
    (let [target {:some-string      "correct"
                  :some-number      50
                  :some-large-value 10}
          calculate-problems
          (sv-core/problem-calculator ::some-object)]
      (is (= [] (calculate-problems target)))))

  (testing "when one top level field is invalid"
    (let [target {:some-string      "correct"
                  :some-number      "oops"
                  :some-large-value 10}
          calculate-problems
          (sv-core/problem-calculator ::some-object)]
      (is (= [{:subject      :some-object
               :field        [:some-number]
               :type         :invalid
               :requirements [:must-be-a-number]}]
            (calculate-problems target)))))

  (testing "when a top level field is specified with spec/and and is invalid"
    (let [target {:some-complex-thing "abc"}
          calculate-problems
          (sv-core/problem-calculator ::some-complex-object)]
      (is (= [{:subject      :some-complex-object
               :field        [:some-complex-thing]
               :type         :invalid
               :requirements [:must-have-length-greater-than-5]}]
            (calculate-problems target)))))

  (testing "when one top level field is missing"
    (let [target {:some-string      "correct"
                  :some-large-value 10}
          calculate-problems
          (sv-core/problem-calculator ::some-object)]
      (is (= [{:subject      :some-object
               :field        [:some-number]
               :type         :missing
               :requirements [:must-be-present]}]
            (calculate-problems target)))))

  (testing "when many top level fields are invalid"
    (let [target {:some-string      10
                  :some-number      "oops"
                  :some-large-value 10}
          calculate-problems
          (sv-core/problem-calculator ::some-object)]
      (is (= [{:subject      :some-object
               :field        [:some-string]
               :type         :invalid
               :requirements [:must-be-a-string]}
              {:subject      :some-object
               :field        [:some-number]
               :type         :invalid
               :requirements [:must-be-a-number]}]
            (calculate-problems target)))))

  (testing "when many top level fields are missing"
    (let [target {:some-large-value 10}
          calculate-problems
          (sv-core/problem-calculator ::some-object)]
      (is (= [{:subject      :some-object
               :field        [:some-string]
               :type         :missing
               :requirements [:must-be-present]}
              {:subject      :some-object
               :field        [:some-number]
               :type         :missing
               :requirements [:must-be-present]}]
            (calculate-problems target)))))

  (testing "when one nested field is invalid"
    (let [target {:some-object  {:some-string      10
                                 :some-number      10
                                 :some-large-value 10}
                  :other-object {:other-string "correct"
                                 :other-number 20}}
          calculate-problems
          (sv-core/problem-calculator ::higher-order-object)]
      (is (= [{:subject      :higher-order-object
               :field        [:some-object :some-string]
               :type         :invalid
               :requirements [:must-be-a-string]}]
            (calculate-problems target)))))

  (testing "when one nested field is missing"
    (let [target {:some-object  {:some-number      10
                                 :some-large-value 10}
                  :other-object {:other-string "correct"
                                 :other-number 20}}
          calculate-problems
          (sv-core/problem-calculator ::higher-order-object)]
      (is (= [{:subject      :higher-order-object
               :field        [:some-object :some-string]
               :type         :missing
               :requirements [:must-be-present]}]
            (calculate-problems target)))))

  (testing "when many nested fields are invalid"
    (let [target {:some-object  {:some-string      "10"
                                 :some-number      "10"
                                 :some-large-value 10}
                  :other-object {:other-string 10
                                 :other-number 10}}
          calculate-problems
          (sv-core/problem-calculator ::higher-order-object)]
      (is (= [{:subject      :higher-order-object
               :field        [:some-object :some-number]
               :type         :invalid
               :requirements [:must-be-a-number]}
              {:subject      :higher-order-object
               :field        [:other-object :other-string]
               :type         :invalid
               :requirements [:must-be-a-string]}]
            (calculate-problems target)))))

  (testing "when many nested fields are missing"
    (let [target {:some-object  {:some-number      10
                                 :some-large-value 10}
                  :other-object {:other-string "correct"}}
          calculate-problems
          (sv-core/problem-calculator ::higher-order-object)]
      (is (= [{:subject      :higher-order-object
               :field        [:some-object :some-string]
               :type         :missing
               :requirements [:must-be-present]}
              {:subject      :higher-order-object
               :field        [:other-object :other-number]
               :type         :missing
               :requirements [:must-be-present]}]
            (calculate-problems target)))))

  (testing "allows validation subject to be overridden"
    (let [target {:some-string      "correct"
                  :some-number      "oops"
                  :some-large-value 10}
          calculate-problems
          (sv-core/problem-calculator ::some-object
            {:validation-subject :the-object})]
      (is (= [{:subject      :the-object
               :field        [:some-number]
               :type         :invalid
               :requirements [:must-be-a-number]}]
            (calculate-problems target)))))

  (testing "allows a problem transformer to be provided"
    (let [target {:some-string      "correct"
                  :some-number      "oops"
                  :some-large-value 10}
          calculate-problems
          (sv-core/problem-calculator ::some-object
            {:problem-transformer
             (fn [problem]
               (merge
                 (select-keys problem [:subject :field :requirements])
                 {:type    :validation-failure
                  :problem (:type problem)}))})]
      (is (= [{:type         :validation-failure
               :subject      :some-object
               :field        [:some-number]
               :problem      :invalid
               :requirements [:must-be-a-number]}]
            (calculate-problems target))))))
