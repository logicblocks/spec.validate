(ns spec.definition.collection.core-test
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]

   [spec.validate.core :as sv-core]

   [spec.definition.collection.core :as sd-collection]

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

(deftest not-empty?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any non-empty countable"
       :samples ["this string is not empty"
                 '(1 2 3)
                 [4 5 6]
                 #{7 8 9}
                 {:ten 11 :twelve 13}])
     (sv-cases/false-case "any empty countable"
       :samples ["" '() [] #{} {}])
     (sv-cases/false-case "non-countables"
       :samples [true (fn []) 26 45.9 10M])
     (sv-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sd-collection/not-empty?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest not-empty?-as-requirement
  (is (= :must-not-be-empty
        (sv-core/pred-requirement
          'spec.definition.collection.core/not-empty?))))

(deftest empty?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any empty countable"
       :samples ["" '() [] #{} {}])
     (sv-cases/false-case "any non-empty countable"
       :samples ["this string is not empty"
                 '(1 2 3)
                 [4 5 6]
                 #{7 8 9}
                 {:ten 11 :twelve 13}])
     (sv-cases/false-case "non-countables"
       :samples [true (fn []) 26 45.9 10M])
     (sv-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sd-collection/empty?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest empty?-as-requirement
  (is (= :must-be-empty
        (sv-core/pred-requirement
          'spec.definition.collection.core/empty?)))
  (is (= :must-be-empty
        (sv-core/pred-requirement
          'clojure.core/empty?))))

(deftest length-equal-to-x?-as-predicate
  (doseq
   [x (range 1 11)
    case
    [(sv-cases/true-case (str "any countable with " x " item(s)")
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
     (sv-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred (ns-resolve
                 (find-ns 'spec.definition.collection.core)
                 (symbol (str "length-equal-to-" x "?")))]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest length-equal-to-x?-as-requirement
  (doseq
   [x (range 1 11)]
    (is (= (keyword (str "must-have-length-equal-to-" x))
          (sv-core/pred-requirement
            (symbol
              "spec.definition.collection.core"
              (str "length-equal-to-" x "?")))))))

(deftest length-less-than-x?-as-predicate
  (doseq
   [x (range 1 11)
    case
    [(sv-cases/true-case (str "any countable with less than " x " item(s)")
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
     (sv-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred (ns-resolve
                 (find-ns 'spec.definition.collection.core)
                 (symbol (str "length-less-than-" x "?")))]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest length-less-than-x?-as-requirement
  (doseq
   [x (range 1 11)]
    (is (= (keyword (str "must-have-length-less-than-" x))
          (sv-core/pred-requirement
            (symbol
              "spec.definition.collection.core"
              (str "length-less-than-" x "?")))))))

(deftest length-greater-than-x?-as-predicate
  (doseq
   [x (range 1 11)
    case
    [(sv-cases/true-case (str "any countable with more than " x " item(s)")
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
     (sv-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred (ns-resolve
                 (find-ns 'spec.definition.collection.core)
                 (symbol (str "length-greater-than-" x "?")))]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest length-greater-than-x?-as-requirement
  (doseq
   [x (range 1 11)]
    (is (= (keyword (str "must-have-length-greater-than-" x))
          (sv-core/pred-requirement
            (symbol
              "spec.definition.collection.core"
              (str "length-greater-than-" x "?")))))))
