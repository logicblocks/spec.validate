(ns spec.validate.string
  (:refer-clojure :exclude [string?])
  (:require
    [clojure.spec.alpha :as spec]
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as string]

    [spec.validate.core :as sv-core]
    [spec.validate.unicode :as sv-unicode]
    [spec.validate.utils :as sv-utils]))

(defn string?
  "Returns true if the provided value is a string, else returns false."
  [value]
  (clojure.core/string? value))

(defmethod sv-core/pred-requirement
  'spec.validate.string/string?
  [_]
  :must-be-a-string)

(defmethod sv-core/pred-requirement
  'clojure.core/string?
  [_]
  :must-be-a-string)

(extend (type string?)
  spec/Specize
  {:specize*
   (fn
     ([f] (spec/specize* f string?))
     ([_ form]
      (spec/with-gen
        (spec/spec form)
        #(gen/gen-for-pred clojure.core/string?))))})

(defn blank?
  "Returns true if the provided value is an empty string or a string containing
  only whitespace characters, else returns false."
  [value]
  (sv-utils/exception->false
    (and (not (nil? value)) (string/blank? value))))

(defmethod sv-core/pred-requirement
  'spec.validate.string/blank?
  [_]
  :must-be-a-blank-string)

(defn- gen-character-string-unicode [pattern]
  (let [ranges (sv-unicode/unicode-codepoint-ranges pattern)
        generators (map #(apply gen/choose %) ranges)]
    (gen/fmap
      (fn [^long i] (Character/toString i))
      (apply gen/one-of [generators]))))

(defn- gen-string-unicode [pattern]
  (gen/fmap clojure.string/join
    (gen/vector (gen-character-string-unicode pattern))))

(extend (type blank?)
  spec/Specize
  {:specize*
   (fn
     ([f] (spec/specize* f blank?))
     ([_ form]
      (spec/with-gen
        (spec/spec form)
        #(gen-string-unicode sv-unicode/whitespace-pattern))))})

(defn not-blank?
  "Returns true if the provided value is a string containing non-whitespace
  characters, else returns false."
  [value]
  (sv-utils/exception->false (not (string/blank? value))))

(defmethod sv-core/pred-requirement
  'spec.validate.string/not-blank?
  [_]
  :must-be-a-non-blank-string)

(def ^:private digits-regex #"^\d*$")

(defn digits?
  "Returns true if the provided value is a string containing zero or more
  characters in the set [0-9], else returns false."
  [value]
  (sv-utils/exception->false (boolean (re-matches digits-regex value))))

(defmethod sv-core/pred-requirement
  'spec.validate.string/digits?
  [_]
  :must-be-a-string-of-digits)

(def ^:private lowercase-alphabetical-regex #"^[a-z]*$")

(defn lowercase-alphabetical?
  "Returns true if the provided value is a string containing zero or more
  characters in the set [a-z], else returns false."
  [value]
  (sv-utils/exception->false
    (boolean (re-matches lowercase-alphabetical-regex value))))

(defmethod sv-core/pred-requirement
  'spec.validate.string/lowercase-alphabetical?
  [_]
  :must-be-a-lowercase-string-of-alphabetical-characters)

(def ^:private uppercase-alphabetical-regex #"^[A-Z]*$")

(defn uppercase-alphabetical?
  "Returns true if the provided value is a string containing zero or more
  characters in the set [A-Z], else returns false."
  [value]
  (sv-utils/exception->false
    (boolean (re-matches uppercase-alphabetical-regex value))))

(defmethod sv-core/pred-requirement
  'spec.validate.string/uppercase-alphabetical?
  [_]
  :must-be-an-uppercase-string-of-alphabetical-characters)

(def ^:private alphabetical-regex #"^[a-zA-Z]*$")

(defn alphabetical?
  "Returns true if the provided value is a string containing zero or more
  characters in the set [a-zA-Z], else returns false."
  [value]
  (sv-utils/exception->false
    (boolean (re-matches alphabetical-regex value))))

(defmethod sv-core/pred-requirement
  'spec.validate.string/alphabetical?
  [_]
  :must-be-a-string-of-alphabetical-characters)
