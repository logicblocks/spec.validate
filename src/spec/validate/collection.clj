(ns spec.validate.collection
  (:require
    [spec.validate.core :as sv-core]
    [spec.validate.utils :as sv-utils]))

(defn not-empty?
  "Returns true if the provided value has a count of at least 1,
  else returns false."
  [value]
  (sv-utils/exception->false (>= (count value) 1)))

(defmethod sv-core/pred-requirement 'spec.validate.predicates/not-empty?
  [_]
  :must-not-be-empty)

(defn length-equal-to?
  "Returns a function that returns true if the provided value has the
  specified length, else returns false."
  [length]
  (fn [value]
    (sv-utils/exception->false (= (count value) length))))

(def length-equal-to-1? (length-equal-to? 1))
(def length-equal-to-2? (length-equal-to? 2))
(def length-equal-to-3? (length-equal-to? 3))
(def length-equal-to-4? (length-equal-to? 4))
(def length-equal-to-5? (length-equal-to? 5))
(def length-equal-to-6? (length-equal-to? 6))
(def length-equal-to-7? (length-equal-to? 7))
(def length-equal-to-8? (length-equal-to? 8))
(def length-equal-to-9? (length-equal-to? 9))
(def length-equal-to-10? (length-equal-to? 10))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-1?
  [_]
  :must-have-length-equal-to-1)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-2?
  [_]
  :must-have-length-equal-to-2)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-3?
  [_]
  :must-have-length-equal-to-3)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-4?
  [_]
  :must-have-length-equal-to-4)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-5?
  [_]
  :must-have-length-equal-to-5)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-6?
  [_]
  :must-have-length-equal-to-6)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-7?
  [_]
  :must-have-length-equal-to-7)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-8?
  [_]
  :must-have-length-equal-to-8)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-9?
  [_]
  :must-have-length-equal-to-9)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-equal-to-10?
  [_]
  :must-have-length-equal-to-10)

(defn length-less-than?
  "Returns a function that returns true if the provided value has a length
  less than the specified length, else returns false."
  [length]
  (fn [value]
    (sv-utils/exception->false (and (some? value) (< (count value) length)))))

(def length-less-than-1? (length-less-than? 1))
(def length-less-than-2? (length-less-than? 2))
(def length-less-than-3? (length-less-than? 3))
(def length-less-than-4? (length-less-than? 4))
(def length-less-than-5? (length-less-than? 5))
(def length-less-than-6? (length-less-than? 6))
(def length-less-than-7? (length-less-than? 7))
(def length-less-than-8? (length-less-than? 8))
(def length-less-than-9? (length-less-than? 9))
(def length-less-than-10? (length-less-than? 10))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-1?
  [_]
  :must-have-length-less-than-1)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-2?
  [_]
  :must-have-length-less-than-2)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-3?
  [_]
  :must-have-length-less-than-3)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-4?
  [_]
  :must-have-length-less-than-4)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-5?
  [_]
  :must-have-length-less-than-5)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-6?
  [_]
  :must-have-length-less-than-6)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-7?
  [_]
  :must-have-length-less-than-7)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-8?
  [_]
  :must-have-length-less-than-8)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-9?
  [_]
  :must-have-length-less-than-9)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-less-than-10?
  [_]
  :must-have-length-less-than-10)

(defn length-greater-than?
  "Returns a function that returns true if the provided value has a length
  greater than the specified length, else returns false."
  [length]
  (fn [value]
    (sv-utils/exception->false (and (some? value) (> (count value) length)))))

(def length-greater-than-1? (length-greater-than? 1))
(def length-greater-than-2? (length-greater-than? 2))
(def length-greater-than-3? (length-greater-than? 3))
(def length-greater-than-4? (length-greater-than? 4))
(def length-greater-than-5? (length-greater-than? 5))
(def length-greater-than-6? (length-greater-than? 6))
(def length-greater-than-7? (length-greater-than? 7))
(def length-greater-than-8? (length-greater-than? 8))
(def length-greater-than-9? (length-greater-than? 9))
(def length-greater-than-10? (length-greater-than? 10))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-1?
  [_]
  :must-have-length-greater-than-1)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-2?
  [_]
  :must-have-length-greater-than-2)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-3?
  [_]
  :must-have-length-greater-than-3)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-4?
  [_]
  :must-have-length-greater-than-4)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-5?
  [_]
  :must-have-length-greater-than-5)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-6?
  [_]
  :must-have-length-greater-than-6)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-7?
  [_]
  :must-have-length-greater-than-7)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-8?
  [_]
  :must-have-length-greater-than-8)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-9?
  [_]
  :must-have-length-greater-than-9)
(defmethod sv-core/pred-requirement
  'spec.validate.predicates/length-greater-than-10?
  [_]
  :must-have-length-greater-than-10)