(ns datatype.collection.core
  (:refer-clojure :exclude [empty?])
  (:require
   [datatype.support :as dts]))

(declare
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

(defn make-length-predicate
  [length callback]
  (fn [value]
    (dts/exception->false
      (and (some? value)
        (callback (count value) length)))))

(defn make-equal-to-length-predicate
  "Returns a function that returns true if the provided value has the
  specified length, else returns false."
  [length]
  (make-length-predicate length =))

(defn make-less-than-length-predicate
  "Returns a function that returns true if the provided value has a length
  less than the specified length, else returns false."
  [length]
  (make-length-predicate length <))

(defn make-greater-than-length-predicate
  "Returns a function that returns true if the provided value has a length
  greater than the specified length, else returns false."
  [length]
  (make-length-predicate length >))

(defmacro def-length-predicate [pred-type desc len pred-maker]
  (let [name (symbol (str "length-" (name pred-type) "-" len "?"))
        doc (str "Returns true if the provided value has a count of " desc " "
              len ", else returns false.")]
    `(let [pred# (~pred-maker ~len)]
       (defn ~name ~doc
         [value#]
         (pred# value#)))))

(defmacro def-length-equal-to-predicate [len]
  `(def-length-predicate :equal-to "exactly" ~len
     make-equal-to-length-predicate))

(defmacro def-length-less-than-predicate [len]
  `(def-length-predicate :less-than "less than" ~len
     make-less-than-length-predicate))

(defmacro def-length-greater-than-predicate [len]
  `(def-length-predicate :greater-than "greater than" ~len
     make-greater-than-length-predicate))

(defn not-empty?
  "Returns true if the provided value has a count of at least 1, else returns
  false."
  [value]
  (dts/exception->false
    (and (some? value)
      (>= (count value) 1))))

(defn empty?
  "Returns true if the provided value has a count of zero, else returns
  false."
  [value]
  (dts/exception->false
    (and (some? value)
      (zero? (count value)))))

(def-length-equal-to-predicate 1)
(def-length-equal-to-predicate 2)
(def-length-equal-to-predicate 3)
(def-length-equal-to-predicate 4)
(def-length-equal-to-predicate 5)
(def-length-equal-to-predicate 6)
(def-length-equal-to-predicate 7)
(def-length-equal-to-predicate 8)
(def-length-equal-to-predicate 9)
(def-length-equal-to-predicate 10)

(def-length-less-than-predicate 1)
(def-length-less-than-predicate 2)
(def-length-less-than-predicate 3)
(def-length-less-than-predicate 4)
(def-length-less-than-predicate 5)
(def-length-less-than-predicate 6)
(def-length-less-than-predicate 7)
(def-length-less-than-predicate 8)
(def-length-less-than-predicate 9)
(def-length-less-than-predicate 10)

(def-length-greater-than-predicate 1)
(def-length-greater-than-predicate 2)
(def-length-greater-than-predicate 3)
(def-length-greater-than-predicate 4)
(def-length-greater-than-predicate 5)
(def-length-greater-than-predicate 6)
(def-length-greater-than-predicate 7)
(def-length-greater-than-predicate 8)
(def-length-greater-than-predicate 9)
(def-length-greater-than-predicate 10)
