(ns spec.definition.collection.core-test
  (:require
   [clojure.spec.alpha :as spec]
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]

   [spec.validate.core :as sv-core]
   [spec.definition.collection.core]

   [datatype.testing.cases :as dt-test-cases]))

(defn gen-string-of-length
  ([length]
   (gen/fmap string/join
     (gen/vector (gen/char-ascii) length)))
  ([min max]
   (gen/fmap string/join
     (gen/vector (gen/char-ascii) min max))))

(defn gen-seq-of-length
  ([length]
   (gen/fmap (partial map str)
     (gen/vector (gen/uuid) length)))
  ([min max]
   (gen/fmap (partial map str)
     (gen/vector (gen/uuid) min max))))

(defn gen-entries-of-length
  ([length]
   (gen/vector
     (gen/tuple
       (gen/fmap keyword
         (gen/fmap string/join
           (gen/vector (gen/char-ascii) 10)))
       (gen/large-integer))
     length))
  ([min max]
   (gen/vector
     (gen/tuple
       (gen/fmap keyword
         (gen/fmap string/join
           (gen/vector (gen/char-ascii) 10)))
       (gen/large-integer))
     min max)))

(deftest non-empty-spec-validation
  (dt-test-cases/assert-cases-satisfied-by
    (partial spec/valid? :datatype.collection/non-empty)
    (dt-test-cases/true-case "any non-empty countable"
      :samples ["this string is not empty"
                '(1 2 3)
                [4 5 6]
                #{7 8 9}
                {:ten 11 :twelve 13}])
    (dt-test-cases/false-case "any empty countable"
      :samples ["" '() [] #{} {}])
    (dt-test-cases/false-case "non-countables"
      :samples [true (fn []) 26 45.9 10M])
    (dt-test-cases/false-case "nil" :sample nil)))

(deftest non-empty-pred-requirement
  (is (= :must-not-be-empty
        (sv-core/pred-requirement
          'datatype.collection.core/not-empty?))))

(deftest empty-spec-validation
  (dt-test-cases/assert-cases-satisfied-by
    (partial spec/valid? :datatype.collection/empty)
    (dt-test-cases/true-case "any empty countable"
      :samples ["" '() [] #{} {}])
    (dt-test-cases/false-case "any non-empty countable"
      :samples ["this string is not empty"
                '(1 2 3)
                [4 5 6]
                #{7 8 9}
                {:ten 11 :twelve 13}])
    (dt-test-cases/false-case "non-countables"
      :samples [true (fn []) 26 45.9 10M])
    (dt-test-cases/false-case "nil" :sample nil)))

(deftest empty-pred-requirement
  (is (= :must-be-empty
        (sv-core/pred-requirement
          'datatype.collection.core/empty?)))
  (is (= :must-be-empty
        (sv-core/pred-requirement
          'clojure.core/empty?))))

(deftest length-equal-to-x-spec-validation
  (doseq
   [x (range 1 11)]
    (dt-test-cases/assert-cases-satisfied-by
      (partial spec/valid?
        (keyword "datatype.collection" (str "length-equal-to-" x)))
      (dt-test-cases/true-case (str "any countable with " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length x))
                       items (gen/generate (gen-seq-of-length x))
                       entries (gen/generate (gen-entries-of-length x))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (dt-test-cases/false-case
        (str "any countable with less than " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length 0 (- x 1)))
                       items (gen/generate (gen-seq-of-length 0 (- x 1)))
                       entries (gen/generate (gen-entries-of-length (- x 1)))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (dt-test-cases/false-case
        (str "any countable with more than " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length (+ x 1) 25))
                       items (gen/generate (gen-seq-of-length (+ x 1) 25))
                       entries (gen/generate (gen-entries-of-length (+ x 1)))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (dt-test-cases/false-case "non-countables"
        :samples [true (fn []) 26 45.9 10M])
      (dt-test-cases/false-case "nil" :sample nil))))

(deftest length-equal-to-x-pred-requirement
  (doseq
   [x (range 1 11)]
    (is (= (keyword (str "must-have-length-equal-to-" x))
          (sv-core/pred-requirement
            (symbol
              "datatype.collection.core"
              (str "length-equal-to-" x "?")))))))

(deftest length-less-than-x-spec-validation
  (doseq
   [x (range 1 11)]
    (dt-test-cases/assert-cases-satisfied-by
      (partial spec/valid?
        (keyword "datatype.collection" (str "length-less-than-" x)))
      (dt-test-cases/true-case
        (str "any countable with less than " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length 0 (- x 1)))
                       items (gen/generate (gen-seq-of-length 0 (- x 1)))
                       entries (gen/generate (gen-entries-of-length (- x 1)))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (dt-test-cases/false-case (str "any countable with " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length x))
                       items (gen/generate (gen-seq-of-length x))
                       entries (gen/generate (gen-entries-of-length x))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (dt-test-cases/false-case
        (str "any countable with more than " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length (+ x 1) 25))
                       items (gen/generate (gen-seq-of-length (+ x 1) 25))
                       entries (gen/generate (gen-entries-of-length (+ x 1)))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (dt-test-cases/false-case "non-countables"
        :samples [true (fn []) 26 45.9 10M])
      (dt-test-cases/false-case "nil" :sample nil))))

(deftest length-less-than-x-pred-requirement
  (doseq
   [x (range 1 11)]
    (is (= (keyword (str "must-have-length-less-than-" x))
          (sv-core/pred-requirement
            (symbol
              "datatype.collection.core"
              (str "length-less-than-" x "?")))))))

(deftest length-greater-than-x-spec-validation
  (doseq
   [x (range 1 11)]
    (dt-test-cases/assert-cases-satisfied-by
      (partial spec/valid?
        (keyword "datatype.collection" (str "length-greater-than-" x)))
      (dt-test-cases/true-case
        (str "any countable with more than " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length (+ x 1) 25))
                       items (gen/generate (gen-seq-of-length (+ x 1) 25))
                       entries (gen/generate (gen-entries-of-length (+ x 1)))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (dt-test-cases/false-case (str "any countable with " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length x))
                       items (gen/generate (gen-seq-of-length x))
                       entries (gen/generate (gen-entries-of-length x))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (dt-test-cases/false-case
        (str "any countable with less than " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length 0 (- x 1)))
                       items (gen/generate (gen-seq-of-length 0 (- x 1)))
                       entries (gen/generate (gen-entries-of-length (- x 1)))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (dt-test-cases/false-case "non-countables"
        :samples [true (fn []) 26 45.9 10M])
      (dt-test-cases/false-case "nil" :sample nil))))

(deftest length-greater-than-x-pred-requirement
  (doseq
   [x (range 1 11)]
    (is (= (keyword (str "must-have-length-greater-than-" x))
          (sv-core/pred-requirement
            (symbol
              "datatype.collection.core"
              (str "length-greater-than-" x "?")))))))
