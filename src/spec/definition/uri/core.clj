(ns spec.definition.uri.core
  (:require
   [datatype.uri.core :as dt-uri]
   [datatype.uri.gen :as dt-uri-gen]

   [spec.definition.core :as sd]))

(sd/def-spec :datatype.uri/http-uri-string
  {:pred dt-uri/http-uri-string?
   :gen #(dt-uri-gen/gen-absolute-uri {:schemes #{:http :https}})
   :req :must-be-an-http-uri-string})
