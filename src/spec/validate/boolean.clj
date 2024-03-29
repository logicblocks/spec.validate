(ns spec.validate.boolean
  (:refer-clojure :exclude [boolean?])
  (:require
   [spec.validate.core :as sv-core]))

(defn boolean?
  "Returns true if the provided value is a boolean, else returns false."
  [value]
  (clojure.core/boolean? value))

(defmethod sv-core/pred-requirement 'spec.validate.predicates/boolean?
  [_]
  :must-be-a-boolean)
