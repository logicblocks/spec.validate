(ns spec.validate.identifier
  (:require
   [clojure.spec.gen.alpha :as gen]
   [spec.validate.utils :as sv-utils]))

(declare
  uuid-string?)

(def ^:private uuid-pattern
  (let [hex-pattern "[a-fA-F0-9]"]
    (re-pattern
      (str "^"
        hex-pattern "{8}" "-"
        hex-pattern "{4}" "-"
        hex-pattern "{4}" "-"
        hex-pattern "{4}" "-"
        hex-pattern "{12}" "$"))))

(defn gen-uuid-string []
  (gen/fmap str (gen/gen-for-pred uuid?)))

(sv-utils/def-validate-pred uuid-string?
  "Returns true if the provided value is an ISO-4217 currency code string,
  else returns false."
  [value]
  {:requirement :must-be-a-uuid-string
   :gen gen-uuid-string}
  (sv-utils/exception->false
    (sv-utils/re-satisfies? uuid-pattern value)))
