(ns spec.validate.time
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [cljc.java-time.offset-date-time :as jt-odt]
   [cljc.java-time.zoned-date-time :as jt-zdt]
   [cljc.java-time.zone-id :as jt-zi]
   [cljc.java-time.zone-offset :as jt-zo]

   [spec.validate.utils :as sv-utils])
  (:import
   [com.google.i18n.phonenumbers PhoneNumberUtil NumberParseException]))

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

(sv-utils/def-validate-pred iso8601-zoned-datetime-string?
  "Returns true if the provided value is a string representing an ISO8601
  zoned datetime, else returns false."
  [value]
  {:requirement :must-be-an-iso8601-zoned-datetime-string
   :gen         #(gen/fmap
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
                     (gen/boolean)))}
  (sv-utils/exception->false
    (boolean (jt-odt/parse value))))

(comment
  (sort (filter #(string/starts-with? % "Pacific") (jt-zi/get-available-zone-ids)))
  (def offset-date-time-utc (jt-odt/now (jt-zi/of "Etc/UTC")))
  (jt-odt/to-string
    (jt-odt/with-offset-same-instant
      offset-date-time-utc
      (jt-zo/of "+03:00")))
  (jt-odt/to-string offset-date-time-utc)
  (jt-zdt/to-string (jt-zdt/now (jt-zi/of "Pacific/Auckland"))))
