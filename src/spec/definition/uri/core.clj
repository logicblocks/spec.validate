(ns spec.definition.uri.core
  (:require
   [datatype.uri.core :as dt-uri]
   [datatype.uri.gen :as dt-uri-gen]

   [spec.definition.core :as sd]))

(declare
  http-uri-string?)

(sd/def-validate-pred http-uri-string?
  "Returns true if the provided value is a string representing a URI with an
  http or https scheme, else returns false."
  [value]
  {:requirement :must-be-an-http-uri-string
   :gen         #(dt-uri-gen/gen-absolute-uri {:schemes #{:http :https}})}
  (dt-uri/http-uri-string? value))
