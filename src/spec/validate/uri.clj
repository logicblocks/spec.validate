(ns spec.validate.uri
  (:require
   [lambdaisland.uri :as uri]

   [spec.validate.core :as sv-core]
   [spec.validate.utils :as sv-utils]))

;; URLs
(defn absolute-url?
  "Returns true if the provided value is a string representing an absolute URL,
  else returns false."
  [value]
  (sv-utils/exception->false (uri/absolute? (uri/uri value))))

(defmethod sv-core/pred-requirement 'spec.validate.predicates/absolute-url?
  [_]
  :must-be-an-absolute-url)
