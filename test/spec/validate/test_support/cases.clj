(ns spec.validate.test-support.cases
  (:import
   [java.util Locale]))

(defn- coll-from-single-or-plural [m singular plural]
  (if (contains? m singular)
    [(singular m)]
    (plural m)))

(defn- test-case [satisfied? title options]
  {:satisfied? satisfied?
   :title      title
   :samples    (coll-from-single-or-plural options :sample :samples)
   :locale     (get options :locale (Locale/UK))
   :notes      (coll-from-single-or-plural options :note :notes)})

(defn true-case [title & {:as options}]
  (test-case true title options))

(defn false-case [title & {:as options}]
  (test-case false title options))
