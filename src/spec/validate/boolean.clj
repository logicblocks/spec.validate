(ns spec.validate.boolean
  (:refer-clojure :exclude [boolean?])
  (:require
   [clojure.spec.gen.alpha :as gen]

   [clojure.string :as string]

   [spec.validate.utils :as sv-utils]))

(declare
  boolean?
  boolean-string?)

(sv-utils/extend-pred-with-requirement
  'clojure.core/boolean?
  :must-be-a-boolean)

(sv-utils/def-validate-pred boolean?
  "Returns true if the provided value is a boolean, else returns false."
  [value]
  {:requirement :must-be-a-boolean
   :gen #(gen/gen-for-pred clojure.core/boolean?)}
  (sv-utils/exception->false
    (clojure.core/boolean? value)))

(sv-utils/def-validate-pred boolean-string?
  "Returns true if the provided value is a string representing a boolean value
  (\"true\" or \"false\", case insensitive), else returns false."
  [value]
  {:requirement :must-be-a-boolean-string
   :gen #(gen/fmap
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
             (gen/boolean)))}
  (sv-utils/exception->false
    (and (string? value)
      (clojure.core/boolean?
        (Boolean/valueOf ^String value)))))
