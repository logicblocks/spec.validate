(ns spec.validate.test-support.string)

(defn whitespace? [^long value]
  (Character/isWhitespace value))

(defn non-whitespace? [^long value]
  (not (whitespace? value)))
