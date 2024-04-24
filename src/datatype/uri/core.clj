(ns datatype.uri.core
  (:require
   [datatype.support :as ds]

   [lambdaisland.uri :as uri]))

(defn http-uri-string? [value]
  (ds/exception->false
    (let [uri (uri/uri value)]
      (and (uri/absolute? uri)
        (contains? #{"http" "https"} (:scheme uri))))))
