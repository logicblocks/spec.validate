(ns datatype.string.gen
  (:require
   [clojure.spec.gen.alpha :as s-gen]
   [clojure.test.check.generators :as tc-gen]
   [clojure.string :as string]

   [icu4clj.text.unicode-set :as icu-tus]
   [icu4clj.text.unicode-set-patterns :as icu-tusp]))

(defn gen-character-string-unicode [pattern]
  (let [ranges (icu-tus/codepoint-ranges pattern)
        generators (map #(apply s-gen/choose %) ranges)]
    (s-gen/fmap
      (fn [^long i] (Character/toString i))
      (apply s-gen/one-of [generators]))))

(defn gen-string-unicode
  ([pattern] (gen-string-unicode pattern {}))
  ([pattern {:keys [min-length max-length allow-empty?]
             :or   {allow-empty? true}}]
   (let [character-gen (gen-character-string-unicode pattern)
         vector-gen
         (cond
           (and min-length max-length)
           (s-gen/vector character-gen min-length max-length)

           max-length
           (s-gen/vector (gen-character-string-unicode pattern) 0 max-length)

           min-length
           (tc-gen/sized
             (fn [size]
               (s-gen/vector character-gen
                 min-length (+ size min-length))))

           :else
           (s-gen/vector character-gen))
         vector-gen
         (if allow-empty? vector-gen (s-gen/not-empty vector-gen))]
     (s-gen/fmap string/join vector-gen))))

(defmacro def-string-gens [suffix pattern]
  (let [string-fn-name#
        (symbol (str "gen-string-" (name suffix)))
        character-fn-name#
        (symbol (str "gen-character-string-" (name suffix)))]
    `(do
       (defn ~string-fn-name#
         ([]
          (gen-string-unicode ~pattern))
         ([options#]
          (gen-string-unicode ~pattern options#)))

       (defn ~character-fn-name# []
         (gen-character-string-unicode ~pattern)))))

(declare
  gen-string-whitespace
  gen-string-non-whitespace
  gen-string-ascii-digits
  gen-string-lowercase-ascii-alphabetics
  gen-string-uppercase-ascii-alphabetics
  gen-string-ascii-alphabetics
  gen-string-lowercase-ascii-alphanumerics
  gen-string-uppercase-ascii-alphanumerics
  gen-string-ascii-alphanumerics

  gen-character-string-whitespace
  gen-character-string-non-whitespace
  gen-character-string-ascii-digits
  gen-character-string-lowercase-ascii-alphabetics
  gen-character-string-uppercase-ascii-alphabetics
  gen-character-string-ascii-alphabetics
  gen-character-string-lowercase-ascii-alphanumerics
  gen-character-string-uppercase-ascii-alphanumerics
  gen-character-string-ascii-alphanumerics)

(def-string-gens whitespace
  icu-tusp/whitespace-pattern)
(def-string-gens non-whitespace
  icu-tusp/non-whitespace-pattern)
(def-string-gens ascii-digits
  icu-tusp/ascii-digit-pattern)
(def-string-gens lowercase-ascii-alphabetics
  icu-tusp/lowercase-ascii-alphabetic-pattern)
(def-string-gens uppercase-ascii-alphabetics
  icu-tusp/uppercase-ascii-alphabetic-pattern)
(def-string-gens ascii-alphabetics
  icu-tusp/ascii-alphabetic-pattern)
(def-string-gens lowercase-ascii-alphanumerics
  icu-tusp/lowercase-ascii-alphanumeric-pattern)
(def-string-gens uppercase-ascii-alphanumerics
  icu-tusp/uppercase-ascii-alphanumeric-pattern)
(def-string-gens ascii-alphanumerics
  icu-tusp/ascii-alphanumeric-pattern)
