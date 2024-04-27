(ns datatype.domain.gen
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [datatype.domain.core :as dt-domain]
   [datatype.string.gen :as dt-string-gen]))

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
  (let [alpha-digit-gen
        (dt-string-gen/gen-character-string-lowercase-ascii-alphanumerics)
        alpha-digit-hyphen-gen
        (dt-string-gen/gen-string-unicode alpha-digit-hyphen-pattern
          {:min-length 0 :max-length 12})]
    (gen/fmap
      string/join
      (gen/frequency
        [[1 alpha-digit-gen]
         [20
          (gen/tuple
            alpha-digit-gen
            alpha-digit-hyphen-gen
            alpha-digit-gen)]]))))

(defn gen-domain-name []
  (gen/fmap
    (fn [[sld tld]]
      (str sld "." tld))
    (gen/tuple
      (gen-lower-level-domain-label)
      (gen-top-level-domain-label))))
