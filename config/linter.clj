(disable-warning
  {:linter :suspicious-expression
   :for-macro 'clojure.core/or
   :if-inside-macroexpansion-of #{'clojure.spec/every 'clojure.spec.alpha/every
                                  'clojure.spec/and 'clojure.spec.alpha/and
                                  'clojure.spec/keys 'clojure.spec.alpha/keys
                                  'clojure.spec/coll-of 'clojure.spec.alpha/coll-of}
   :within-depth 6
   :reason "clojure.spec's macros `keys`, `every`, and `and` often contain `clojure.core/and` invocations with only one argument."})

(disable-warning
  {:linter :constant-test
   :for-macro 'clojure.core/if
   :if-inside-macroexpansion-of #{'spec.definition.core/def-validate-pred}
   :within-depth 10})
