(ns spec.definition.time.core
  (:require
   [datatype.time.core :as dt-time]
   [datatype.time.gen :as dt-time-gen]

   [spec.definition.core :as sd]))

(sd/def-spec :datatype.time/iso8601-zoned-datetime-string
  {:pred dt-time/iso8601-zoned-datetime-string?
   :gen dt-time-gen/gen-iso8601-zoned-datetime-string
   :req :must-be-an-iso8601-zoned-datetime-string})
