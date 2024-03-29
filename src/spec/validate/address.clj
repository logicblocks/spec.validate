(ns spec.validate.address
  (:require
   [spec.validate.core :as sv-core]
   [spec.validate.utils :as sv-utils]))

(def ^:private postcode-regex
  (re-pattern
    (str "^([A-Za-z][A-Ha-hK-Yk-y]?[0-9][A-Za-z0-9]? "
      "[0-9][A-Za-z]{2}|[Gg][Ii][Rr] 0[Aa]{2})$")))

(defn postcode?
  "Returns true if the provided value is a string representing a UK postcode,
  else returns false."
  [value]
  (sv-utils/exception->false (boolean (re-matches postcode-regex value))))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/postcode?
  [_]
  :must-be-a-uk-postcode)
