(ns spec.definition.core
  (:require
   [clojure.spec.alpha :as spec]

   [spec.validate.core :as sv-core])
  (:import [java.util.regex Pattern]))

(defmacro exception->false [form]
  `(try ~form (catch Exception _# false)))

(defn extend-pred-with-gen
  [pred gen]
  (extend (type pred)
    spec/Specize
    {:specize*
     (fn
       ([f]
        (spec/specize* f pred))
       ([f form]
        (spec/with-gen (spec/spec form) gen)))}))

(defn extend-pred-with-requirement
  [sym requirement]
  (defmethod sv-core/pred-requirement
    sym [_] requirement))

(defmacro def-spec [k opts]
  (let [{:keys [pred gen req]} opts]
    `(do
       (spec/def ~k (spec/spec ~pred :gen ~gen))
       (extend-pred-with-requirement '~(symbol (resolve pred)) ~req))))
