(ns datatype.phone.core
  (:require
   [datatype.support :as dts])
  (:import
   [com.google.i18n.phonenumbers
    PhoneNumberUtil
    PhoneNumberUtil$PhoneNumberFormat
    Phonenumber$PhoneNumber]))

(def ^:dynamic *default-region-code* "GB")

(defn- phone-number-format [format]
  (let [formats
        {:international PhoneNumberUtil$PhoneNumberFormat/INTERNATIONAL
         :national      PhoneNumberUtil$PhoneNumberFormat/NATIONAL
         :e164          PhoneNumberUtil$PhoneNumberFormat/E164
         :rfc3966       PhoneNumberUtil$PhoneNumberFormat/RFC3966}]
    (formats format)))

(defn- phone-number-util ^PhoneNumberUtil []
  (PhoneNumberUtil/getInstance))

(defn- parse-phone-number
  [^Phonenumber$PhoneNumber phone-number ^String default-region-code]
  (.parse (phone-number-util) phone-number default-region-code))

(defn- valid-phone-number [^Phonenumber$PhoneNumber phone-number]
  (.isValidNumber (phone-number-util) phone-number))

(defn phone-number-string? [value]
  (dts/exception->false
    (valid-phone-number
      (parse-phone-number value *default-region-code*))))
