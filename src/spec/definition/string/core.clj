(ns spec.definition.string.core
  (:refer-clojure :exclude [string?])
  (:require
   [clojure.spec.gen.alpha :as gen]

   [datatype.string.core :as dt-string]
   [datatype.string.gen :as dt-string-gen]

   [spec.definition.core :as sd]))

(sd/def-spec :datatype.string/string
  {:pred clojure.core/string?
   :gen #(gen/gen-for-pred clojure.core/string?)
   :req :must-be-a-string})

(sd/def-spec :datatype.string/blank
  {:pred dt-string/blank?
   :gen dt-string-gen/gen-string-whitespace
   :req :must-be-a-blank-string})

(sd/def-spec :datatype.string/non-blank
  {:pred dt-string/not-blank?
   :gen dt-string-gen/gen-string-non-whitespace
   :req :must-be-a-non-blank-string})

(sd/def-spec :datatype.string/ascii-digits
  {:pred dt-string/ascii-digits?
   :gen dt-string-gen/gen-string-ascii-digits
   :req :must-be-a-string-of-ascii-digits})

(sd/def-spec :datatype.string/lowercase-ascii-alphabetics
  {:pred dt-string/lowercase-ascii-alphabetics?
   :gen dt-string-gen/gen-string-lowercase-ascii-alphabetics
   :req :must-be-a-string-of-lowercase-ascii-alphabetic-characters})

(sd/def-spec :datatype.string/uppercase-ascii-alphabetics
  {:pred dt-string/uppercase-ascii-alphabetics?
   :gen dt-string-gen/gen-string-uppercase-ascii-alphabetics
   :req :must-be-a-string-of-uppercase-ascii-alphabetic-characters})

(sd/def-spec :datatype.string/ascii-alphabetics
  {:pred dt-string/ascii-alphabetics?
   :gen dt-string-gen/gen-string-ascii-alphabetics
   :req :must-be-a-string-of-ascii-alphabetic-characters})

(sd/def-spec :datatype.string/lowercase-ascii-alphanumerics
  {:pred dt-string/lowercase-ascii-alphanumerics?
   :gen dt-string-gen/gen-string-lowercase-ascii-alphanumerics
   :req :must-be-a-string-of-lowercase-ascii-alphanumeric-characters})

(sd/def-spec :datatype.string/uppercase-ascii-alphanumerics
  {:pred dt-string/uppercase-ascii-alphanumerics?
   :gen dt-string-gen/gen-string-uppercase-ascii-alphanumerics
   :req :must-be-a-string-of-uppercase-ascii-alphanumeric-characters})

(sd/def-spec :datatype.string/ascii-alphanumerics
  {:pred dt-string/ascii-alphanumerics?
   :gen dt-string-gen/gen-string-ascii-alphanumerics
   :req :must-be-a-string-of-ascii-alphanumeric-characters})
