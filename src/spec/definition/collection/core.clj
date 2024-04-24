(ns spec.definition.collection.core
  (:refer-clojure :exclude [empty?])
  (:require
   [spec.definition.core :as sd]))

(declare
  empty?
  not-empty?

  length-equal-to?
  length-equal-to-1?
  length-equal-to-2?
  length-equal-to-3?
  length-equal-to-4?
  length-equal-to-5?
  length-equal-to-6?
  length-equal-to-7?
  length-equal-to-8?
  length-equal-to-9?
  length-equal-to-10?

  length-less-than?
  length-less-than-1?
  length-less-than-2?
  length-less-than-3?
  length-less-than-4?
  length-less-than-5?
  length-less-than-6?
  length-less-than-7?
  length-less-than-8?
  length-less-than-9?
  length-less-than-10?

  length-greater-than?
  length-greater-than-1?
  length-greater-than-2?
  length-greater-than-3?
  length-greater-than-4?
  length-greater-than-5?
  length-greater-than-6?
  length-greater-than-7?
  length-greater-than-8?
  length-greater-than-9?
  length-greater-than-10?)

(sd/def-validate-pred not-empty?
  "Returns true if the provided value has a count of at least 1, else returns
  false."
  [value]
  {:requirement :must-not-be-empty}
  (sd/exception->false
    (and (some? value)
      (>= (count value) 1))))

(sd/def-validate-pred empty?
  "Returns true if the provided value has a count of zero, else returns
  false."
  [value]
  {:requirement :must-be-empty}
  (sd/exception->false
    (and (some? value)
      (zero? (count value)))))

(sd/extend-pred-with-requirement
  'clojure.core/empty?
  :must-be-empty)

(defn make-equal-to-length-predicate
  "Returns a function that returns true if the provided value has the
  specified length, else returns false."
  [length]
  (fn [value]
    (sd/exception->false
      (and (some? value)
        (= (count value) length)))))

(defmacro def-length-equal-to-pred [len]
  (let [name (symbol (str "length-equal-to-" len "?"))
        doc (str "Returns true if the provided value has a count of exactly "
              len ", else returns false.")
        req (keyword (str "must-have-length-equal-to-" len))]
    `(let [pred# (make-equal-to-length-predicate ~len)]
       (sd/def-validate-pred ~name
         ~doc
         [value#]
         {:requirement ~req}
         (pred# value#)))))

(def-length-equal-to-pred 1)
(def-length-equal-to-pred 2)
(def-length-equal-to-pred 3)
(def-length-equal-to-pred 4)
(def-length-equal-to-pred 5)
(def-length-equal-to-pred 6)
(def-length-equal-to-pred 7)
(def-length-equal-to-pred 8)
(def-length-equal-to-pred 9)
(def-length-equal-to-pred 10)

(defn make-less-than-length-predicate
  "Returns a function that returns true if the provided value has a length
  less than the specified length, else returns false."
  [length]
  (fn [value]
    (sd/exception->false
      (and (some? value)
        (< (count value) length)))))

(defmacro def-length-less-than-pred [len]
  (let [name (symbol (str "length-less-than-" len "?"))
        doc (str "Returns true if the provided value has a count less than "
              len ", else returns false.")
        req (keyword (str "must-have-length-less-than-" len))]
    `(let [pred# (make-less-than-length-predicate ~len)]
       (sd/def-validate-pred ~name
         ~doc
         [value#]
         {:requirement ~req}
         (pred# value#)))))

(def-length-less-than-pred 1)
(def-length-less-than-pred 2)
(def-length-less-than-pred 3)
(def-length-less-than-pred 4)
(def-length-less-than-pred 5)
(def-length-less-than-pred 6)
(def-length-less-than-pred 7)
(def-length-less-than-pred 8)
(def-length-less-than-pred 9)
(def-length-less-than-pred 10)

(defn make-greater-than-length-predicate
  "Returns a function that returns true if the provided value has a length
  greater than the specified length, else returns false."
  [length]
  (fn [value]
    (sd/exception->false
      (and (some? value)
        (> (count value) length)))))

(defmacro def-length-greater-than-pred [len]
  (let [name (symbol (str "length-greater-than-" len "?"))
        doc (str "Returns true if the provided value has a count greater than "
              len ", else returns false.")
        req (keyword (str "must-have-length-greater-than-" len))]
    `(let [pred# (make-greater-than-length-predicate ~len)]
       (sd/def-validate-pred ~name
         ~doc
         [value#]
         {:requirement ~req}
         (pred# value#)))))

(def-length-greater-than-pred 1)
(def-length-greater-than-pred 2)
(def-length-greater-than-pred 3)
(def-length-greater-than-pred 4)
(def-length-greater-than-pred 5)
(def-length-greater-than-pred 6)
(def-length-greater-than-pred 7)
(def-length-greater-than-pred 8)
(def-length-greater-than-pred 9)
(def-length-greater-than-pred 10)
