(ns spec.validate.address
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [spec.validate.string :as sv-string]
   [spec.validate.utils :as sv-utils]))

(declare
  uk-postcode-formatted-string?)

(def ^:private uk-postcode-pattern
  (re-pattern
    (str "^[A-Za-z]{1,2}[0-9][A-Za-z0-9]? ?[0-9][A-Za-z]{2}$")))

(defn gen-uk-postcode-formatted-string []
  (let [gen-alpha-string (partial sv-string/gen-string-unicode "[A-Z]")
        gen-numeric-string (partial sv-string/gen-string-unicode "[0-9]")
        gen-alphanumeric-string
        (partial sv-string/gen-string-unicode "[A-Z0-9]")]
    (gen/fmap
      (fn [[area district sector unit space? lowercase?]]
        (cond-> (str area district)
          space? (str " ")
          true (str sector unit)
          lowercase? (string/lower-case)))
      (gen/tuple
        (gen-alpha-string {:min-length 1 :max-length 2})
        (gen/fmap string/join
          (gen/tuple
            (gen-numeric-string {:min-length 1 :max-length 1})
            (gen-alphanumeric-string {:min-length 0 :max-length 1})))
        (gen-numeric-string {:min-length 1 :max-length 1})
        (gen-alpha-string {:min-length 2 :max-length 2})
        (gen/boolean)
        (gen/boolean)))))

(sv-utils/def-validate-pred uk-postcode-formatted-string?
  "Returns true if the provided value is a string representing a UK postcode,
  else returns false."
  [value]
  {:requirement :must-be-a-uk-postcode
   :gen         gen-uk-postcode-formatted-string}
  (sv-utils/exception->false
    (sv-utils/re-satisfies? uk-postcode-pattern value)))
