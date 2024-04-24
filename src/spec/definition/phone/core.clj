(ns spec.definition.phone.core
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [datatype.phone.core :as dt-phone]

   [spec.definition.core :as sd-def])
  (:import
   [com.google.i18n.phonenumbers
    PhoneNumberUtil
    PhoneNumberUtil$PhoneNumberFormat
    Phonenumber$PhoneNumber]))

(declare
  phone-number-string?)

(def ^:dynamic *region-codes*
  ["AC" "AD" "AE" "AF" "AG" "AI" "AL" "AM" "AO" "AR" "AS" "AT" "AU" "AW" "AX"
   "AZ" "BA" "BB" "BD" "BE" "BF" "BG" "BH" "BI" "BJ" "BL" "BM" "BN" "BO" "BQ"
   "BR" "BS" "BT" "BW" "BY" "BZ" "CA" "CC" "CD" "CF" "CG" "CH" "CI" "CK" "CL"
   "CM" "CN" "CO" "CR" "CU" "CV" "CW" "CX" "CY" "CZ" "DE" "DJ" "DK" "DM" "DO"
   "DZ" "EC" "EE" "EG" "EH" "ER" "ES" "ET" "FI" "FJ" "FK" "FM" "FO" "FR" "GA"
   "GB" "GD" "GE" "GF" "GG" "GH" "GI" "GL" "GM" "GN" "GP" "GQ" "GR" "GT" "GU"
   "GW" "GY" "HK" "HN" "HR" "HT" "HU" "ID" "IE" "IL" "IM" "IN" "IO" "IQ" "IR"
   "IS" "IT" "JE" "JM" "JO" "JP" "KE" "KG" "KH" "KI" "KM" "KN" "KP" "KR" "KW"
   "KY" "KZ" "LA" "LB" "LC" "LI" "LK" "LR" "LS" "LT" "LU" "LV" "LY" "MA" "MC"
   "MD" "ME" "MF" "MG" "MH" "MK" "ML" "MM" "MN" "MO" "MP" "MQ" "MR" "MS" "MT"
   "MU" "MV" "MW" "MX" "MY" "MZ" "NA" "NC" "NE" "NF" "NG" "NI" "NL" "NO" "NP"
   "NR" "NU" "NZ" "OM" "PA" "PE" "PF" "PG" "PH" "PK" "PL" "PM" "PR" "PS" "PT"
   "PW" "PY" "QA" "RE" "RO" "RS" "RU" "RW" "SA" "SB" "SC" "SD" "SE" "SG" "SH"
   "SI" "SJ" "SK" "SL" "SM" "SN" "SO" "SR" "SS" "ST" "SV" "SX" "SY" "SZ" "TA"
   "TC" "TD" "TG" "TH" "TJ" "TK" "TL" "TM" "TN" "TO" "TR" "TT" "TV" "TW" "TZ"
   "UA" "UG" "US" "UY" "UZ" "VA" "VC" "VE" "VG" "VI" "VN" "VU" "WF" "WS" "XK"
   "YE" "YT" "ZA" "ZM" "ZW"])

(def ^:dynamic *default-region-code* "GB")

(defn- phone-number-util ^PhoneNumberUtil []
  (PhoneNumberUtil/getInstance))

(defn- example-number [region-code]
  (.getExampleNumber (phone-number-util) region-code))

(defn- number-of-leading-zeros [^Phonenumber$PhoneNumber phone-number]
  (.getNumberOfLeadingZeros phone-number))

(defn- national-significant-number [^Phonenumber$PhoneNumber phone-number]
  (.getNationalSignificantNumber (phone-number-util) phone-number))

(defn- country-code [^Phonenumber$PhoneNumber phone-number]
  (.getCountryCode phone-number))

(defn- phone-number-format [format]
  (let [formats
        {:international PhoneNumberUtil$PhoneNumberFormat/INTERNATIONAL
         :national      PhoneNumberUtil$PhoneNumberFormat/NATIONAL
         :e164          PhoneNumberUtil$PhoneNumberFormat/E164
         :rfc3966       PhoneNumberUtil$PhoneNumberFormat/RFC3966}]
    (formats format)))

(defn- parse-phone-number
  [^Phonenumber$PhoneNumber phone-number ^String default-region-code]
  (.parse (phone-number-util) phone-number default-region-code))

(defn- format-phone-number
  [^Phonenumber$PhoneNumber phone-number format]
  (.format (phone-number-util) phone-number (phone-number-format format)))

(defn gen-phone-number
  ([] (gen-phone-number {}))
  ([{:keys [region-code]}]
   (gen/bind
     (gen/tuple
       (gen/boolean)
       (gen/boolean)
       (if region-code
         (gen/return region-code)
         (gen/elements *region-codes*)))
     (fn [[local? formatted? region-code]]
       (let [region-code (if local? *default-region-code* region-code)
             example (example-number region-code)
             leading-zeros (number-of-leading-zeros example)
             national-length (count (national-significant-number example))]
         (gen/fmap
           (fn [national-part-digits]
             (let [pieces
                   (concat
                     (if local?
                       (repeat leading-zeros "0")
                       ["+" (country-code example)])
                     national-part-digits)
                   joined
                   (string/join pieces)]
               (if formatted?
                 (format-phone-number
                   (parse-phone-number joined region-code)
                   (if local? :national :international))
                 joined)))
           (gen/vector
             (gen/elements
               ["1" "2" "3" "4" "5" "6" "7" "8" "9"])
             national-length)))))))

(sd-def/def-validate-pred phone-number-string?
  "Returns true if the provided value is a string representing a phone number,
  else returns false. By default, treats phone numbers as being from
  Great Britain, however the default region can be overridden with
  `*default-region-code*`."
  [value]
  {:requirement :must-be-a-phone-number-string
   :gen         gen-phone-number}
  (dt-phone/phone-number-string? value))
