(ns spec.definition.core
  (:require
   [clojure.spec.alpha :as spec]

   [spec.validate.core :as sv-core])
  (:import [java.util.regex Pattern]))

(defmacro exception->false [form]
  `(try ~form (catch Exception _# false)))

(defn re-satisfies? [re s]
  (not (nil? (re-find re s))))

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

(defmacro def-validate-pred
  [sym doc-string params options & body]
  `(do
     (defn ~sym ~doc-string ~params
       ~@body)

     (extend-pred-with-requirement
       (symbol
         (str (ns-name *ns*))
         ~(name sym))
       ~(:requirement options))

     (when ~(:gen options)
       (extend-pred-with-gen ~sym
         ~(:gen options)))))

(defmacro def-spec [k opts]
  (let [{:keys [pred gen req]} opts]
    `(do
       (spec/def ~k (spec/spec ~pred :gen ~gen))
       (extend-pred-with-requirement '~(symbol (resolve pred)) ~req))))