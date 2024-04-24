(ns datatype.uuid.core
  (:require
   [datatype.support :as dts]))

(def uuid-pattern
  (let [hex-pattern "[a-fA-F0-9]"]
    (re-pattern
      (str
        hex-pattern "{8}" "-"
        hex-pattern "{4}" "-"
        hex-pattern "{4}" "-"
        hex-pattern "{4}" "-"
        hex-pattern "{12}"))))

(defn uuid-string? [value]
  (dts/exception->false
    (dts/re-satisfies?
      (dts/re-exact-pattern uuid-pattern)
      value)))
