(ns spec.validate.network
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]))

(defn gen-ipv4-address-string []
  (gen/fmap
    (partial string/join ".")
    (gen/tuple
      (gen/choose 0 255)
      (gen/choose 0 255)
      (gen/choose 0 255)
      (gen/choose 0 255))))
