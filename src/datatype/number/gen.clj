(ns datatype.number.gen
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]

   [icu4clj.text.number-format :as icu-tnf]))

(defn gen-integer-string []
  (gen/fmap str
    (gen/gen-for-pred clojure.core/integer?)))

(defn gen-decimal-string []
  (let [{:keys [digits
                zero-digit
                plus-sign
                minus-sign
                decimal-separator]}
        (icu-tnf/decimal-format-symbols)]
    (gen/fmap
      (fn [[sign integer fraction
            include-integer? include-fraction? corrector]]
        (let [include-neither?
              (and
                (not include-integer?)
                (not include-fraction?))
              include-integer? (if include-neither?
                                 (= corrector :integer)
                                 include-integer?)
              include-fraction? (if include-neither?
                                  (= corrector :fraction)
                                  include-fraction?)
              integer
              (if include-integer?
                (string/join integer)
                "")
              fraction
              (if include-fraction?
                (string/join fraction)
                "")]
          (str sign integer decimal-separator fraction)))
      (gen/tuple
        (gen/elements ["" minus-sign plus-sign])
        (gen/one-of
          [(gen/return zero-digit)
           (gen/vector (gen/elements digits) 1 100)])
        (gen/vector (gen/elements digits) 1 100)
        (gen/boolean)
        (gen/boolean)
        (gen/elements [:integer :fraction])))))
