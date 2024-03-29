(ns spec.validate.test-support.cases)

(defmacro true-case [title & {:as options}]
  {:title      title
   :samples    (if (contains? options :sample)
                 [(:sample options)]
                 (:samples options))
   :satisfied? true})
(defmacro false-case [title & {:as options}]
  {:title      title
   :samples    (if (contains? options :sample)
                 [(:sample options)]
                 (:samples options))
   :satisfied? false})
