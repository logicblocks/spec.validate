(ns spec.validate.email
  (:require
    [valip.predicates :as valip-predicates]

    [spec.validate.core :as sv-core]
    [spec.validate.utils :as sv-utils]))

(defn email-address?
  "Returns true if the email address is valid, based on RFC 2822. Email
  addresses containing quotation marks or square brackets are considered
  invalid, as this syntax is not commonly supported in practise. The domain of
  the email address is not checked for validity."
  [value]
  (sv-utils/exception->false (valip-predicates/email-address? value)))

(defmethod sv-core/pred-requirement
  'spec.validate.predicates/email-address?
  [_]
  :must-be-an-email-address)
