(ns spec.validate.phone
  (:require
   [spec.validate.core :as sv-core]
   [spec.validate.utils :as sv-utils])
  (:import
   [com.google.i18n.phonenumbers PhoneNumberUtil NumberParseException]))

(def ^:dynamic *default-phone-number-region-code* "GB")

(def ^:private ^PhoneNumberUtil phone-number-util
  (PhoneNumberUtil/getInstance))

(defn- string->PhoneNumber [value]
  (try
    (.parse phone-number-util value *default-phone-number-region-code*)
    (catch NumberParseException _
      nil)))

(defn phone-number?
  "Returns true if the provided value is a string representing a phone number,
  else returns false. By default, treats phone numbers as being from
  Great Britain, however the default region can be overridden with
  `*default-phone-number-region-code*`."
  [value]
  (sv-utils/exception->false
    (.isValidNumber phone-number-util (string->PhoneNumber value))))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/phone-number?
  [_]
  :must-be-a-phone-number)
