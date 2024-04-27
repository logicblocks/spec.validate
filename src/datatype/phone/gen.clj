(ns datatype.phone.gen
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [datatype.phone.core :as dt-phone]))

(defn gen-phone-number
  ([] (gen-phone-number {}))
  ([{:keys [region-code]}]
   (gen/bind
     (gen/tuple
       (gen/boolean)
       (gen/boolean)
       (if region-code
         (gen/return region-code)
         (gen/elements dt-phone/*region-codes*)))
     (fn [[local? formatted? region-code]]
       (let [region-code (if local? dt-phone/*default-region-code* region-code)
             example (dt-phone/example-number region-code)
             leading-zeros (dt-phone/number-of-leading-zeros example)
             national-length
             (count (dt-phone/national-significant-number example))]
         (gen/fmap
           (fn [national-part-digits]
             (let [pieces
                   (concat
                     (if local?
                       (repeat leading-zeros "0")
                       ["+" (dt-phone/country-code example)])
                     national-part-digits)
                   joined
                   (string/join pieces)]
               (if formatted?
                 (dt-phone/format-phone-number
                   (dt-phone/parse-phone-number joined region-code)
                   (if local? :national :international))
                 joined)))
           (gen/vector
             (gen/elements
               ["1" "2" "3" "4" "5" "6" "7" "8" "9"])
             national-length)))))))
