(ns spec.definition.bool.core
  (:require
   [clojure.spec.gen.alpha :as gen]

   [datatype.bool.core :as dt-bool]
   [datatype.bool.gen :as dt-bool-gen]

   [spec.definition.core :as sd]))

(sd/def-spec :datatype.bool/boolean
  {:pred clojure.core/boolean?
   :gen #(gen/gen-for-pred clojure.core/boolean?)
   :req :must-be-a-boolean})

(sd/def-spec :datatype.bool/boolean-string
  {:pred dt-bool/boolean-string?
   :gen dt-bool-gen/gen-boolean-string
   :req :must-be-a-boolean-string})
