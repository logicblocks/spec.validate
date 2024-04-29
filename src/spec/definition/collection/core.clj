(ns spec.definition.collection.core
  (:refer-clojure :exclude [empty?])
  (:require
   [datatype.collection.core :as dt-collection]

   [spec.definition.core :as sd]))

(sd/def-spec :datatype.collection/non-empty
  {:pred dt-collection/not-empty?
   :req :must-not-be-empty})

(sd/def-spec :datatype.collection/empty
  {:pred dt-collection/empty?
   :req :must-be-empty})

(sd/extend-pred-with-requirement
  'clojure.core/empty?
  :must-be-empty)

(defmacro def-length-equal-to-pred [len pred]
  (let [k (keyword "datatype.collection" (str "length-equal-to-" len))
        req (keyword (str "must-have-length-equal-to-" len))
        pred (symbol "datatype.collection.core"
               (str "length-equal-to-" len "?"))]
    `(sd/def-spec ~k
       {:pred ~pred
        :req ~req})))

(def-length-equal-to-pred 1 dt-collection/length-equal-to-1?)
(def-length-equal-to-pred 2 dt-collection/length-equal-to-2?)
(def-length-equal-to-pred 3 dt-collection/length-equal-to-3?)
(def-length-equal-to-pred 4 dt-collection/length-equal-to-4?)
(def-length-equal-to-pred 5 dt-collection/length-equal-to-5?)
(def-length-equal-to-pred 6 dt-collection/length-equal-to-6?)
(def-length-equal-to-pred 7 dt-collection/length-equal-to-7?)
(def-length-equal-to-pred 8 dt-collection/length-equal-to-8?)
(def-length-equal-to-pred 9 dt-collection/length-equal-to-9?)
(def-length-equal-to-pred 10 dt-collection/length-equal-to-10?)

(defmacro def-length-less-than-pred [len pred]
  (let [k (keyword "datatype.collection" (str "length-less-than-" len))
        req (keyword (str "must-have-length-less-than-" len))
        pred (symbol "datatype.collection.core"
               (str "length-less-than-" len "?"))]
    `(sd/def-spec ~k
       {:pred ~pred
        :req ~req})))

(def-length-less-than-pred 1 dt-collection/length-less-than-1?)
(def-length-less-than-pred 2 dt-collection/length-less-than-2?)
(def-length-less-than-pred 3 dt-collection/length-less-than-3?)
(def-length-less-than-pred 4 dt-collection/length-less-than-4?)
(def-length-less-than-pred 5 dt-collection/length-less-than-5?)
(def-length-less-than-pred 6 dt-collection/length-less-than-6?)
(def-length-less-than-pred 7 dt-collection/length-less-than-7?)
(def-length-less-than-pred 8 dt-collection/length-less-than-8?)
(def-length-less-than-pred 9 dt-collection/length-less-than-9?)
(def-length-less-than-pred 10 dt-collection/length-less-than-10?)

(defmacro def-length-greater-than-pred [len pred]
  (let [k (keyword "datatype.collection" (str "length-greater-than-" len))
        req (keyword (str "must-have-length-greater-than-" len))
        pred (symbol "datatype.collection.core"
               (str "length-greater-than-" len "?"))]
    `(sd/def-spec ~k
       {:pred ~pred
        :req ~req})))

(def-length-greater-than-pred 1 dt-collection/length-greater-than-1?)
(def-length-greater-than-pred 2 dt-collection/length-greater-than-2?)
(def-length-greater-than-pred 3 dt-collection/length-greater-than-3?)
(def-length-greater-than-pred 4 dt-collection/length-greater-than-4?)
(def-length-greater-than-pred 5 dt-collection/length-greater-than-5?)
(def-length-greater-than-pred 6 dt-collection/length-greater-than-6?)
(def-length-greater-than-pred 7 dt-collection/length-greater-than-7?)
(def-length-greater-than-pred 8 dt-collection/length-greater-than-8?)
(def-length-greater-than-pred 9 dt-collection/length-greater-than-9?)
(def-length-greater-than-pred 10 dt-collection/length-greater-than-10?)
