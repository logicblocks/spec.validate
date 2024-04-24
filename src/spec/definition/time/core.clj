(ns spec.definition.time.core
  (:require
   [clojure.spec.gen.alpha :as gen]

   [cljc.java-time.offset-date-time :as jt-odt]
   [cljc.java-time.zone-offset :as jt-zo]

   [datatype.time.core :as dt-time]

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

(defn gen-iso8601-zoned-datetime-string []
  (gen/fmap
    (fn [[days-offset minutes-offset timezone-offset past?]]
      (-> (jt-odt/now)
        (jt-odt/with-offset-same-instant
          (jt-zo/of-hours timezone-offset))
        (jt-odt/plus-days
          (if past? (- days-offset) days-offset))
        (jt-odt/plus-minutes
          (if past? (- minutes-offset) minutes-offset))
        (jt-odt/to-string)))
    (gen/tuple
      (gen/large-integer* {:min 0 :max 365})
      (gen/large-integer* {:min 0 :max 10000})
      (gen/large-integer* {:min -12 :max 12})
      (gen/boolean))))

(sd/def-validate-pred iso8601-zoned-datetime-string?
  "Returns true if the provided value is a string representing an ISO8601
  zoned datetime, else returns false."
  [value]
  {:requirement :must-be-an-iso8601-zoned-datetime-string
   :gen         gen-iso8601-zoned-datetime-string}
  (dt-time/iso8601-zoned-datetime-string? value))
