(ns spec.validate.test-support.string)

(defn whitespace? [^long value]
  (Character/isWhitespace value))
