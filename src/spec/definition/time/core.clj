(ns spec.definition.time.core
  (:require
   [datatype.time.core :as dt-time]
   [datatype.time.gen :as dt-time-gen]

   [spec.definition.core :as sd]))

(declare
  ; iso8601-duration-string?
  ; iso8601-interval-string?
  ; iso8601-local-date-string?
  ; iso8601-local-time-string?
  ; iso8601-local-datetime-string?
  iso8601-zoned-datetime-string?

  ; past-iso8601-local-date-string?
  ; past-iso8601-local-datetime-string?
  ; past-iso8601-zoned-datetime-string?

  ; future-iso8601-local-date-string?
  ; future-iso8601-local-datetime-string?
  ; future-iso8601-zoned-datetime-string?
  )

(sd/def-validate-pred iso8601-zoned-datetime-string?
  "Returns true if the provided value is a string representing an ISO8601
  zoned datetime, else returns false."
  [value]
  {:requirement :must-be-an-iso8601-zoned-datetime-string
   :gen         dt-time-gen/gen-iso8601-zoned-datetime-string}
  (dt-time/iso8601-zoned-datetime-string? value))
