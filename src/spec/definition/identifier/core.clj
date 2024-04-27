(ns spec.definition.identifier.core
  (:require
   [datatype.uuid.core :as dt-uuid]
   [datatype.uuid.gen :as dt-uuid-gen]

   [spec.definition.core :as sd]))

(declare
  uuid-string?)

(sd/def-validate-pred uuid-string?
  "Returns true if the provided value is a string representing a UUID,
  else returns false."
  [value]
  {:requirement :must-be-a-uuid-string
   :gen         dt-uuid-gen/gen-uuid-string}
  (dt-uuid/uuid-string? value))
