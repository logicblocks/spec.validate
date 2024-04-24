(ns datatype.bool.core-test
  (:require
   [clojure.test :refer [deftest]]

   [datatype.bool.core :as dt-bool]

   [spec.test-support.cases :as sv-cases]))

(deftest boolean-string?-as-predicate
  (sv-cases/assert-cases-satisfied-by dt-bool/boolean-string?
    (sv-cases/true-case "boolean strings"
      :samples ["true" "false" "TRUE" "FALSE" "True" "False"])
    (sv-cases/false-case "booleans"
      :samples [true false
                Boolean/TRUE Boolean/FALSE
                (Boolean/valueOf true) (Boolean/valueOf false)])
    (sv-cases/false-case "non-strings"
      :samples [35.4 #{"GBP" "USD"}])
    (sv-cases/false-case "nil" :sample nil)))
