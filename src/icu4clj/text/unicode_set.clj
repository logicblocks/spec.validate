(ns icu4clj.text.unicode-set
  (:refer-clojure :exclude [complement])
  (:import [com.ibm.icu.text UnicodeSet]))

(defn ^UnicodeSet unicode-set [^String pattern]
  (doto (UnicodeSet. pattern)
    (.freeze)))

(defn ^UnicodeSet complement [^UnicodeSet unicode-set]
  (.complement unicode-set))

(defn codepoint-ranges [^UnicodeSet unicode-set]
  (into []
    (mapv #(vec [(.codepoint %) (.codepointEnd %)])
      (.ranges unicode-set))))

(defn character-vector [^UnicodeSet unicode-set]
  (vec unicode-set))
