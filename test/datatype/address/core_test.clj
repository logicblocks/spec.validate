(ns datatype.address.core-test
  (:require
   [clojure.test :refer [deftest]]

   [datatype.address.core :as dt-address]

   [spec.test-support.cases :as sv-cases]))

(deftest uk-postcode-formatted-string?
  (sv-cases/assert-cases-satisfied-by dt-address/uk-postcode-formatted-string?
    (sv-cases/true-case "any uppercase standard format UK postcode"
      :samples ["EC1A 1BB"
                "W1A 0AX"
                "M1 1AE"
                "B33 8TH"
                "CR2 6XH"
                "DN55 1PT"])
    (sv-cases/true-case "any lowercase standard format UK postcode"
      :samples ["ec1a 1bb"
                "w1a 0ax"
                "m1 1ae"
                "b33 8th"
                "cr2 6xh"
                "dn55 1pt"])
    ; TODO: should this be false since the Crown Dependencies aren't the UK?
    (sv-cases/true-case "any Crown Dependency postcode"
      :samples ["GY8 0HG"                                   ; Guernsey
                "IM1 5PD"                                   ; Isle of Man
                "JE2 3AB"                                   ; Jersey
                ])
    ; TODO: should this be false since gibraltar is a British Overseas
    ; Territory?
    (sv-cases/true-case "Gibraltar"
      :sample "GX11 1AA"                                    ; Gibraltar
      )
    ; TODO: should this be true or false?
    (sv-cases/true-case "British Forces Post Office post codes"
      :samples ["BF1 1AA"
                "BF1 2AN"])
    (sv-cases/false-case "some British Overseas Territories"
      :samples ["ASCN 1ZZ" ; Ascension island
                "BBND 1ZZ" ; British Indian Ocean Territory
                "BIQQ 1ZZ" ; British Antarctic Territory
                "FIQQ 1ZZ" ; Falkland Islands
                "PCRN 1ZZ" ; Pitcairn Islands
                "SIQQ 1ZZ" ; South Georgia and the South Sandwich Islands
                "STHL 1ZZ" ; Saint Helena
                "TDCU 1ZZ" ; Tristan da Cunha
                "TKCA 1ZZ" ; Turks and Caicos Islands
                "AI-2640"  ; Anguilla
                "KY1-1600" ; Cayman Islands
                "MSR-1110" ; Montserrat
                "VG-1130"  ; British Virgin Islands
                "WK 06"    ; Bermuda
                ])
    (sv-cases/false-case "British Forces Post Office (BFPO) codes"
      :samples ["BFPO 801"])
    (sv-cases/false-case "strings that aren't postcode-like at all"
      :samples ["the quick brown fox jumped over the lazy dog"
                "23.6"
                "true"])
    (sv-cases/false-case "a non-string"
      :samples [true false 35.4 #{"GBP" "USD"}])
    (sv-cases/false-case "nil" :sample nil)))
