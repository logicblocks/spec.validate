(ns spec.definition.uuid.core
  (:require
   [datatype.uuid.core :as dt-uuid]
   [datatype.uuid.gen :as dt-uuid-gen]

   [spec.definition.core :as sd]))

(sd/def-spec :datatype.uuid/uuid-string
  {:pred dt-uuid/uuid-string?
   :gen dt-uuid-gen/gen-uuid-string
   :req :must-be-a-uuid-string})
