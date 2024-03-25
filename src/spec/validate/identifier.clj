(ns spec.validate.identifier
  (:require
    [spec.validate.core :as sv-core]
    [spec.validate.utils :as sv-utils]))

;; identifiers
(def ^:private uuid-v4-regex
  (re-pattern
    (str "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-4[a-fA-F0-9]{3}-"
      "[89abAB][a-fA-F0-9]{3}-[a-fA-F0-9]{12}$")))

(defn uuid-v4?
  "Returns true if the provided value is a string representing a v4 UUID,
  else returns false."
  [value]
  (sv-utils/exception->false (boolean (re-matches uuid-v4-regex value))))

(defmethod sv-core/pred-requirement 'spec.validate.predicates/uuid-v4?
  [_]
  :must-be-a-v4-uuid)
