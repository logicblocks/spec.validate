(ns datatype.phone.core
  (:require
   [datatype.support :as dts])
  (:import
   [com.google.i18n.phonenumbers
    PhoneNumberUtil
    PhoneNumberUtil$PhoneNumberFormat
    Phonenumber$PhoneNumber]))

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

(defn phone-number-format [format]
  (let [formats
        {:international PhoneNumberUtil$PhoneNumberFormat/INTERNATIONAL
         :national      PhoneNumberUtil$PhoneNumberFormat/NATIONAL
         :e164          PhoneNumberUtil$PhoneNumberFormat/E164
         :rfc3966       PhoneNumberUtil$PhoneNumberFormat/RFC3966}]
    (formats format)))

(defn phone-number-util ^PhoneNumberUtil []
  (PhoneNumberUtil/getInstance))

(defn example-number [region-code]
  (.getExampleNumber (phone-number-util) region-code))

(defn number-of-leading-zeros [^Phonenumber$PhoneNumber phone-number]
  (.getNumberOfLeadingZeros phone-number))

(defn national-significant-number [^Phonenumber$PhoneNumber phone-number]
  (.getNationalSignificantNumber (phone-number-util) phone-number))

(defn country-code [^Phonenumber$PhoneNumber phone-number]
  (.getCountryCode phone-number))

(defn parse-phone-number
  [^CharSequence phone-number ^String default-region-code]
  (.parse (phone-number-util) phone-number default-region-code))

(defn format-phone-number
  [^Phonenumber$PhoneNumber phone-number format]
  (.format (phone-number-util) phone-number (phone-number-format format)))

(defn valid-phone-number? [^Phonenumber$PhoneNumber phone-number]
  (.isValidNumber (phone-number-util) phone-number))

(defn phone-number-string? [value]
  (dts/exception->false
    (valid-phone-number?
      (parse-phone-number value *default-region-code*))))
