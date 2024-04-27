(ns datatype.uuid.gen
  (:require
   [clojure.spec.gen.alpha :as gen]))

(defn gen-uuid-string []
  (gen/fmap str (gen/gen-for-pred uuid?)))
