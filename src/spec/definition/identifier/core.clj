(ns spec.definition.identifier.core
  (:require
   [clojure.spec.gen.alpha :as gen]

   [datatype.uuid.core :as dt-uuid]

   [spec.definition.core :as sd]))

(declare
  uuid-string?)

(defn gen-uuid-string []
  (gen/fmap str (gen/gen-for-pred uuid?)))

(sd/def-validate-pred uuid-string?
  "Returns true if the provided value is a string representing a UUID,
  else returns false."
  [value]
  {:requirement :must-be-a-uuid-string
   :gen gen-uuid-string}
  (dt-uuid/uuid-string? value))
