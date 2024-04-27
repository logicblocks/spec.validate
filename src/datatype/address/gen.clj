(ns datatype.address.gen
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [datatype.string.gen :as dt-string-gen]))

(defn gen-uk-postcode-formatted-string []
  (gen/fmap
    (fn [[area district sector unit space? lowercase?]]
      (cond-> (str area district)
        space? (str " ")
        true (str sector unit)
        lowercase? (string/lower-case)))
    (gen/tuple
      (dt-string-gen/gen-string-uppercase-ascii-alphabetics
        {:min-length 1 :max-length 2})
      (gen/fmap string/join
        (gen/tuple
          (dt-string-gen/gen-string-ascii-digits
            {:min-length 1 :max-length 1})
          (dt-string-gen/gen-string-uppercase-ascii-alphanumerics
            {:min-length 0 :max-length 1})))
      (dt-string-gen/gen-string-ascii-digits
        {:min-length 1 :max-length 1})
      (dt-string-gen/gen-string-uppercase-ascii-alphabetics
        {:min-length 2 :max-length 2})
      (gen/boolean)
      (gen/boolean))))
