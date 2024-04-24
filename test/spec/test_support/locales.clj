(ns spec.test-support.locales
  (:import
   [java.util Locale]))

(defmacro with-locale [^Locale locale & body]
  `(let [default-locale# (Locale/getDefault)]
     (try
       (Locale/setDefault ~locale)
       ~@body
       (finally
         (Locale/setDefault default-locale#)))))
