(ns datatype.uri.core
  (:require
   [datatype.support :as ds]

   [lambdaisland.uri :as uri]))

(defn http-uri-string?
  "Returns true if the provided value is a string representing a URI with an
  http or https scheme, else returns false."
  [value]
  (ds/exception->false
    (let [uri (uri/uri value)]
      (and (uri/absolute? uri)
        (contains? #{"http" "https"} (:scheme uri))))))
