(ns datatype.time.core-test
  (:require
   [clojure.test :refer [deftest]]

   [datatype.time.core :as dt-time]

   [spec.test-support.cases :as sv-cases]))

(deftest iso8601-zoned-datetime-string?-as-predicate
  (sv-cases/assert-cases-satisfied-by dt-time/iso8601-zoned-datetime-string?
    (sv-cases/true-case "any extended format UTC offset datetime string"
      :samples ["1986-09-20T16:34:56.123Z"
                "2025-11-10T11:14:56Z"
                "2036-10-15T01:00Z"
                "+12050-12-25T13:00:00.000000Z"])
    (sv-cases/true-case "any extended format non-UTC offset datetime string"
      :samples ["1986-09-20T16:34:56.123+03"
                "2025-11-10T11:14:56+10:00"
                "2036-10-15T01:00+00:00"
                "2036-10-15T01:00:30-08"
                "+12050-12-25T13:00:00.000000-11:00"])
    (sv-cases/false-case "any basic format offset datetime string"
      :samples ["19860920T163456.123Z"
                "20251110T111456Z"
                "20361015T0100Z"
                "+120501225T130000.000000Z"
                "19860920T163456.123+03"
                "20251110T111456+1000"
                "20361015T0100+0000"
                "20361015T010030-08"
                "+120501225T130000.000000-1100"])
    (sv-cases/false-case "any extended format local datetime string"
      :samples ["1986-09-20T16:34:56.123"
                "2025-11-10T11:14:56"
                "2036-10-15T01:00"
                "+12050-12-25T13:00:00.000000"])
    (sv-cases/false-case "any basic format local datetime string"
      :samples ["19860920T163456.123"
                "20251110T111456"
                "20361015T0100"
                "+120501225T130000.000000"])
    (sv-cases/false-case "any extended format zoned datetime string"
      :samples ["1986-09-20T16:34:56.123Z[Etc/UTC]"
                "2025-11-10T11:14:56+03:00[Europe/Athens]"
                "2036-10-15T01:00-04:00[America/New_York]"
                "+12050-12-25T13:00:00.000000+12:00[Pacific/Auckland]"])
    (sv-cases/false-case "any time only string"
      :samples ["T16:34:56.123Z"
                "T11:14:56Z"
                "T01:00Z"
                "T13:00:00.000000Z"
                "T16:34:56.123+03"
                "T11:14:56+10:00"
                "T01:00+00:00"
                "T01:00:30-08"
                "T13:00:00.000000-11:00"])
    (sv-cases/false-case "any date only string"
      :samples ["1986-09-20"
                "2025-11-10"
                "2036-10-15"
                "+12050-12-25"
                "1986-09-20"
                "2025-11-10"
                "2036-10-15"
                "2036-10-15"
                "+12050-12-25"])
    (sv-cases/false-case "any non-datetime string"
      :samples ["spinach" "hello world" "<h2>wat</h2>"])
    (sv-cases/false-case "any non-string"
      :samples [true false 22 25.45 #{1 2 3}])
    (sv-cases/false-case "nil" :sample nil)))
