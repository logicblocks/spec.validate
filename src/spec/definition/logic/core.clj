(ns spec.definition.logic.core
  (:refer-clojure :exclude [boolean?])
  (:require
   [clojure.spec.gen.alpha :as gen]

   [datatype.bool.core :as dt-bool]
   [datatype.bool.gen :as dt-bool-gen]

   [spec.definition.core :as sd]))

(declare
  boolean?
  boolean-string?)

(sd/extend-pred-with-requirement
  'clojure.core/boolean?
  :must-be-a-boolean)

(sd/def-validate-pred boolean?
  "Returns true if the provided value is a boolean, else returns false."
  [value]
  {:requirement :must-be-a-boolean
   :gen         #(gen/gen-for-pred clojure.core/boolean?)}
  (sd/exception->false
    (clojure.core/boolean? value)))

(sd/def-validate-pred boolean-string?
  "Returns true if the provided value is a string representing a boolean value
  (\"true\" or \"false\", case insensitive), else returns false."
  [value]
  {:requirement :must-be-a-boolean-string
   :gen         dt-bool-gen/gen-boolean-string}
  (dt-bool/boolean-string? value))
