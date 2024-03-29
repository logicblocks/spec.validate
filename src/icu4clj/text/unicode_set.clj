(ns icu4clj.text.unicode-set
  (:refer-clojure :exclude [complement])
  (:import
   [com.ibm.icu.text UnicodeSet
    UnicodeSet$EntryRange]))

(defn ^UnicodeSet unicode-set [^String pattern]
  (doto (UnicodeSet. pattern)
    (.freeze)))

(defn ^UnicodeSet complement [^UnicodeSet unicode-set]
  (.complement unicode-set))

(defn codepoint-ranges [^UnicodeSet unicode-set]
  (vec (mapv (fn [^UnicodeSet$EntryRange range]
               (vec [(.codepoint range) (.codepointEnd range)]))
         (.ranges unicode-set))))

(defn character-vector [^UnicodeSet unicode-set]
  (vec unicode-set))
