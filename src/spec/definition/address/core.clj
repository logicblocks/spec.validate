(ns spec.definition.address.core
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [datatype.address.core :as dt-address]

   [spec.definition.core :as sd]
   [spec.definition.string.core :as sd-string]))

(declare
  uk-postcode-formatted-string?)

(defn gen-uk-postcode-formatted-string []
  (let [gen-alpha-string (partial sd-string/gen-string-unicode "[A-Z]")
        gen-numeric-string (partial sd-string/gen-string-unicode "[0-9]")
        gen-alphanumeric-string
        (partial sd-string/gen-string-unicode "[A-Z0-9]")]
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

(sd/def-validate-pred uk-postcode-formatted-string?
  "Returns true if the provided value is a string representing a UK postcode,
  else returns false."
  [value]
  {:requirement :must-be-a-uk-postcode
   :gen         gen-uk-postcode-formatted-string}
  (dt-address/uk-postcode-formatted-string? value))
