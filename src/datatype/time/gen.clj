(ns datatype.time.gen
  (:require
   [clojure.spec.gen.alpha :as gen]

   [cljc.java-time.offset-date-time :as jt-odt]
   [cljc.java-time.zone-offset :as jt-zo]))

(defn gen-iso8601-zoned-datetime-string []
  (gen/fmap
    (fn [[days-offset minutes-offset timezone-offset past?]]
      (-> (jt-odt/now)
        (jt-odt/with-offset-same-instant
          (jt-zo/of-hours timezone-offset))
        (jt-odt/plus-days
          (if past? (- days-offset) days-offset))
        (jt-odt/plus-minutes
          (if past? (- minutes-offset) minutes-offset))
        (jt-odt/to-string)))
    (gen/tuple
      (gen/large-integer* {:min 0 :max 365})
      (gen/large-integer* {:min 0 :max 10000})
      (gen/large-integer* {:min -12 :max 12})
      (gen/boolean))))
