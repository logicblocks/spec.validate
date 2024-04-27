(ns datatype.collection.core-test
  (:refer-clojure :exclude [empty?])
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]
   [clojure.test :refer [deftest]]

   [datatype.collection.core :as dt-collection]

   [spec.test-support.cases :as sv-cases]))

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

(deftest not-empty?
  (sv-cases/assert-cases-satisfied-by dt-collection/not-empty?
    (sv-cases/true-case "any non-empty countable"
      :samples ["this string is not empty"
                '(1 2 3)
                [4 5 6]
                #{7 8 9}
                {:ten 11 :twelve 13}])
    (sv-cases/false-case "any empty countable"
      :samples ["" '() [] #{} {}])
    (sv-cases/false-case "non-countables"
      :samples [true (fn []) 26 45.9 10M])
    (sv-cases/false-case "nil" :sample nil)))

(deftest empty?
  (sv-cases/assert-cases-satisfied-by dt-collection/empty?
    (sv-cases/true-case "any empty countable"
      :samples ["" '() [] #{} {}])
    (sv-cases/false-case "any non-empty countable"
      :samples ["this string is not empty"
                '(1 2 3)
                [4 5 6]
                #{7 8 9}
                {:ten 11 :twelve 13}])
    (sv-cases/false-case "non-countables"
      :samples [true (fn []) 26 45.9 10M])
    (sv-cases/false-case "nil" :sample nil)))

(deftest length-equal-to-x?
  (doseq
   [x (range 1 11)]
    (sv-cases/assert-cases-satisfied-by
      (ns-resolve
        (find-ns 'datatype.collection.core)
        (symbol (str "length-equal-to-" x "?")))
      (sv-cases/true-case (str "any countable with " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length x))
                       items (gen/generate (gen-seq-of-length x))
                       entries (gen/generate (gen-entries-of-length x))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (sv-cases/false-case (str "any countable with less than " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length 0 (- x 1)))
                       items (gen/generate (gen-seq-of-length 0 (- x 1)))
                       entries (gen/generate (gen-entries-of-length (- x 1)))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (sv-cases/false-case (str "any countable with more than " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length (+ x 1) 25))
                       items (gen/generate (gen-seq-of-length (+ x 1) 25))
                       entries (gen/generate (gen-entries-of-length (+ x 1)))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (sv-cases/false-case "non-countables"
        :samples [true (fn []) 26 45.9 10M])
      (sv-cases/false-case "nil" :sample nil))))

(deftest length-less-than-x?
  (doseq
   [x (range 1 11)]
    (sv-cases/assert-cases-satisfied-by
      (ns-resolve
        (find-ns 'datatype.collection.core)
        (symbol (str "length-less-than-" x "?")))
      (sv-cases/true-case (str "any countable with less than " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length 0 (- x 1)))
                       items (gen/generate (gen-seq-of-length 0 (- x 1)))
                       entries (gen/generate (gen-entries-of-length (- x 1)))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (sv-cases/false-case (str "any countable with " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length x))
                       items (gen/generate (gen-seq-of-length x))
                       entries (gen/generate (gen-entries-of-length x))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (sv-cases/false-case (str "any countable with more than " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length (+ x 1) 25))
                       items (gen/generate (gen-seq-of-length (+ x 1) 25))
                       entries (gen/generate (gen-entries-of-length (+ x 1)))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (sv-cases/false-case "non-countables"
        :samples [true (fn []) 26 45.9 10M])
      (sv-cases/false-case "nil" :sample nil))))

(deftest length-greater-than-x?
  (doseq
   [x (range 1 11)]
    (sv-cases/assert-cases-satisfied-by
      (ns-resolve
        (find-ns 'datatype.collection.core)
        (symbol (str "length-greater-than-" x "?")))
      (sv-cases/true-case (str "any countable with more than " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length (+ x 1) 25))
                       items (gen/generate (gen-seq-of-length (+ x 1) 25))
                       entries (gen/generate (gen-entries-of-length (+ x 1)))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (sv-cases/false-case (str "any countable with " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length x))
                       items (gen/generate (gen-seq-of-length x))
                       entries (gen/generate (gen-entries-of-length x))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (sv-cases/false-case (str "any countable with less than " x " item(s)")
        :samples (let [string (gen/generate (gen-string-of-length 0 (- x 1)))
                       items (gen/generate (gen-seq-of-length 0 (- x 1)))
                       entries (gen/generate (gen-entries-of-length (- x 1)))]
                   [string
                    (into '() items)
                    (into [] items)
                    (into #{} items)
                    (into {} entries)]))
      (sv-cases/false-case "non-countables"
        :samples [true (fn []) 26 45.9 10M])
      (sv-cases/false-case "nil" :sample nil))))
