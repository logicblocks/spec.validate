(ns datatype.bool.gen
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]))

(defn gen-boolean-string []
  (gen/fmap
    (fn [[bool c1 c2 c3 c4 c5]]
      (let [cases [c1 c2 c3 c4 c5]]
        (string/join
          (map-indexed
            (fn [index char]
              (if (nth cases index)
                (string/upper-case char)
                char))
            (str bool)))))
    (gen/tuple
      (gen/gen-for-pred clojure.core/boolean?)
      (gen/boolean)
      (gen/boolean)
      (gen/boolean)
      (gen/boolean)
      (gen/boolean))))
