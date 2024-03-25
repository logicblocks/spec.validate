(ns spec.validate.time
  (:require
    [clj-time.format :as datetimes]

    [spec.validate.core :as sv-core]
    [spec.validate.utils :as sv-utils])
  (:import
    [com.google.i18n.phonenumbers PhoneNumberUtil NumberParseException]
    [org.apache.commons.validator.routines DoubleValidator]))

(defn iso8601-datetime?
  "Returns true if the provided value is a string representing an ISO8601
  datetime, else returns false."
  [value]
  (sv-utils/nil->false (and (datetimes/parse value) true)))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/iso8601-datetime?
  [_]
  :must-be-an-iso8601-datetime)
