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
   (let [subject (get options :subject)
         subject (cond
                   (nil? subject) (fn [_ _] (keyword (name spec)))
                   (fn? subject) subject
                   :else (fn [_ _] subject))
         transformer (or (:transformer options) identity)]
     (fn [target]
       (let [context (spec/explain-data spec target)]
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
                 (transformer
                   {:type         type
                    :subject      (subject spec target)
                    :field        path-to-field
                    :requirements [requirement]}))))
           []
           (::spec/problems context)))))))
