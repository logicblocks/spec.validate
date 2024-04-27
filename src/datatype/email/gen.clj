(ns datatype.email.gen
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [datatype.domain.gen :as dt-domain-gen]
   [datatype.string.gen :as dt-string-gen]))

(defn gen-email-address []
  (gen/fmap
    (fn [[local-part domain-part]]
      (str local-part "@" domain-part))
    (gen/tuple
      (gen/fmap
        string/join
        (gen/vector
          (gen/frequency
            [[1 (dt-string-gen/gen-character-string-unicode
                  "[\\^\\&.!#$%'*+/\\=?_`{|}~-]")]
             [9 (dt-string-gen/gen-character-string-ascii-alphanumerics)]])
          1 63))
      (dt-domain-gen/gen-domain-name))))
