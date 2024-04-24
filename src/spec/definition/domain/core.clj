(ns spec.definition.domain.core
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [datatype.domain.core :as dt-domain]

   [spec.definition.string.core :as sd-string]))

(def ^:private alpha-pattern "[a-z]")
(def ^:private digit-pattern "[0-9]")
(def ^:private hyphen-pattern "[-]")
(def ^:private alpha-digit-pattern
  (str "[" alpha-pattern digit-pattern "]"))
(def ^:private alpha-digit-hyphen-pattern
  (str "[" alpha-digit-pattern hyphen-pattern "]"))

(defn gen-top-level-domain-label []
  (gen/elements dt-domain/top-level-domain-labels))

(defn gen-lower-level-domain-label []
  (gen/fmap
    string/join
    (gen/frequency
      [[1 (sd-string/gen-character-string-unicode alpha-digit-pattern)]
       [20 (gen/tuple
             (sd-string/gen-character-string-unicode alpha-digit-pattern)
             (sd-string/gen-string-unicode alpha-digit-hyphen-pattern
               {:min-length 0 :max-length 12})
             (sd-string/gen-character-string-unicode alpha-digit-pattern))]])))

(defn gen-domain-name []
  (gen/fmap
    (fn [[sld tld]]
      (str sld "." tld))
    (gen/tuple
      (gen-lower-level-domain-label)
      (gen-top-level-domain-label))))
