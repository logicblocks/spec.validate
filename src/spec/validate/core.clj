(ns spec.validate.core
  (:require
   [clojure.spec.alpha :as spec]))

(defn- predicate-details [pred]
  (vec (get (vec pred) 2)))
(defn- pred-fn-symbol [pred]
  (get (predicate-details pred) 0))
(defn- pred-fn-field [pred]
  (get (predicate-details pred) 2))

(defmulti pred-error
  (fn [pred]
    (if (seq? pred)
      (pred-fn-symbol pred)
      pred)))
(defmethod pred-error 'clojure.core/contains? [_]
  :missing)
(defmethod pred-error :default [_]
  :invalid)

(defmulti pred-requirement identity)
(defmethod pred-requirement :default [_]
  :must-be-valid)

(defn validator [spec]
  (fn [validation-target]
    (spec/valid? spec validation-target)))

(defn problem-calculator
  ([spec & {:as options}]
   (let [{:keys [validation-subject
                 problem-transformer]
          :or   {validation-subject  (keyword (name spec))
                 problem-transformer identity}} options]
     (fn [validation-target]
       (let [context (spec/explain-data spec validation-target)]
         (reduce
           (fn [accumulator problem]
             (let [pred (:pred problem)
                   is-missing
                   (and
                     (seq? pred)
                     (= 'clojure.core/contains? (pred-fn-symbol pred)))
                   [path-to-field type requirement]
                   (if is-missing
                     [(conj (:in problem) (pred-fn-field pred))
                      :missing
                      :must-be-present]
                     [(:in problem)
                      :invalid
                      (pred-requirement pred)])]
               (conj accumulator
                 (problem-transformer
                   {:type         type
                    :subject      validation-subject
                    :field        path-to-field
                    :requirements [requirement]}))))
           []
           (::spec/problems context)))))))
