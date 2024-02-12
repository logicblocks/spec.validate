(ns spec.validate.utils)

(defmacro exception->false [form]
  `(try ~form (catch Exception _# false)))

(defn- nil->false [value]
  (if (nil? value) false value))
