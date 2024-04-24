(ns spec.definition.logic.core
  (:refer-clojure :exclude [boolean?])
  (:require
   [clojure.spec.gen.alpha :as gen]

   [clojure.string :as string]

   [datatype.bool.core :as dt-bool]

   [spec.definition.core :as sd]))

(declare
  boolean?
  boolean-string?)

(sd/extend-pred-with-requirement
  'clojure.core/boolean?
  :must-be-a-boolean)

(sd/def-validate-pred boolean?
  "Returns true if the provided value is a boolean, else returns false."
  [value]
  {:requirement :must-be-a-boolean
   :gen #(gen/gen-for-pred clojure.core/boolean?)}
  (sd/exception->false
    (clojure.core/boolean? value)))

(defn gen-boolean-string []
  (gen/fmap
    (fn [[bool c1 c2 c3 c4 c5]]
      (let [cases [c1 c2 c3 c4 c5]]
        (string/join
          (map-indexed
            (fn [index char]
              (if (nth cases index)
                (string/upper-case char)
                char))
            (str bool)))))
    (gen/tuple
      (gen/gen-for-pred clojure.core/boolean?)
      (gen/boolean)
      (gen/boolean)
      (gen/boolean)
      (gen/boolean)
      (gen/boolean))))

(sd/def-validate-pred boolean-string?
  "Returns true if the provided value is a string representing a boolean value
  (\"true\" or \"false\", case insensitive), else returns false."
  [value]
  {:requirement :must-be-a-boolean-string
   :gen gen-boolean-string}
  (dt-bool/boolean-string? value))
