(ns spec.validate.utils
  (:require
    [clojure.spec.alpha :as spec]

    [spec.validate.core :as sv-core]))

(defmacro exception->false [form]
  `(try ~form (catch Exception _# false)))

(defn- nil->false [value]
  (if (nil? value) false value))

(defn extend-pred-with-gen
  [pred gen]
  (extend (type pred)
    spec/Specize
    {:specize*
     (fn
       ([f] (spec/specize* f pred))
       ([_ form] (spec/with-gen (spec/spec form) gen)))}))

(defn extend-pred-with-requirement
  [sym requirement]
  (defmethod sv-core/pred-requirement
    sym [_] requirement))
