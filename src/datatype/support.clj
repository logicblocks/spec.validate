(ns datatype.support
  (:import [java.util.regex Pattern]))

(defmacro exception->false [form]
  `(try ~form (catch Exception _# false)))

(defn re-satisfies? [re s]
  (not (nil? (re-find re s))))

(defn re-quote [value]
  (Pattern/quote (str value)))

(defn re-exact-pattern [re]
  (re-pattern (str "^" re "$")))
