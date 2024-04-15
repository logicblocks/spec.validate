(ns spec.validate.uri
  (:require
   [clojure.string :as string]
   [clojure.spec.gen.alpha :as gen]

   [lambdaisland.uri :as uri]
   [lambdaisland.uri.normalize :as uri-norm]

   [spec.validate.string :as sv-string]
   [spec.validate.network :as sv-network]
   [spec.validate.utils :as sv-utils]
   [spec.validate.domain :as sv-domain]
   [spec.validate.uri.scheme :as sv-uri-scheme]))

(declare
  http-uri-string?)

; support username / password
; support fragment
; increase size of query string character set

(def gen-delims-pattern "[\\:/?#\\[\\]@]")
(def sub-delims-pattern "[\\&!$'()*+,;=]")

(def path-non-delims-pattern "[\\:@]")

(def alpha-digit-pattern "[a-z0-9]")
(def unreserved-sym-pattern "[-._~]")

(defn gen-path-segment
  ([] (gen-path-segment {}))
  ([{:keys [min-path-segment-length
            max-path-segment-length]}]
   (gen/fmap
     string/join
     (gen/vector
       (gen/frequency
         [[1 (gen/fmap uri-norm/percent-encode
               (sv-string/gen-character-string-unicode
                 (str "[" gen-delims-pattern "-" path-non-delims-pattern "]")))]
          [1 (sv-string/gen-character-string-unicode
               (str "[" unreserved-sym-pattern sub-delims-pattern "]"))]
          [10 (sv-string/gen-character-string-unicode alpha-digit-pattern)]])
       (or min-path-segment-length 0)
       (or max-path-segment-length 10)))))

(defn gen-absolute-path
  ([] (gen-absolute-path {}))
  ([{:keys [min-path-segments
            max-path-segments]
     :as   options}]
   (gen/fmap
     (fn [[segments empty-path?]]
       (if (seq segments)
         (str "/" (string/join "/" segments))
         (if empty-path? "" "/")))
     (gen/tuple
       (gen/vector (gen-path-segment options)
         (or min-path-segments 0)
         (or max-path-segments 4))
       (gen/boolean)))))

(defn gen-query-map
  ([] (gen-query-map {}))
  ([{:keys [min-query-param-count
            max-query-param-count
            min-query-param-key-length
            max-query-param-key-length
            min-query-param-value-length
            max-query-param-value-length]}]
   (let [key-gen
         (sv-string/gen-string-unicode "[a-z]"
           {:min-length (or min-query-param-key-length 1)
            :max-length (or max-query-param-key-length 10)})
         value-gen
         (sv-string/gen-string-unicode "[a-z]"
           {:min-length (or min-query-param-value-length 1)
            :max-length (or max-query-param-value-length 10)})]
     (gen/fmap
       (fn [entries]
         (into {} entries))
       (gen/vector
         (gen/tuple
           key-gen
           (gen/frequency
             [[1 (gen/vector value-gen 1 3)]
              [9 value-gen]]))
         (or min-query-param-count 0)
         (or max-query-param-count 4))))))

(defn gen-absolute-uri
  ([] (gen-absolute-uri {}))
  ([{:keys [schemes
            include-ipv4-addresses?]
     :or   {include-ipv4-addresses? true}
     :as   options}]
   (let [scheme-gen
         (gen/elements
           (or
             (map name schemes)
             (sv-uri-scheme/uri-schemes)))
         host-gen
         (gen/frequency
           [[(if (true? include-ipv4-addresses?) 1 0)
             (sv-network/gen-ipv4-address-string)]
            [9 (sv-domain/gen-domain-name)]])
         port-gen
         (gen/frequency
           [[1 (gen/large-integer* {:min 0 :max 65535})]
            [9 (gen/return nil)]])
         path-gen
         (gen-absolute-path options)
         query-map-gen
         (gen-query-map options)]
     (gen/fmap
       (fn [[scheme host port path query-map]]
         (uri/uri-str
           {:scheme scheme
            :host   host
            :port   port
            :path   path
            :query  (uri/map->query-string query-map)}))
       (gen/tuple
         scheme-gen
         host-gen
         port-gen
         path-gen
         query-map-gen)))))

(sv-utils/def-validate-pred http-uri-string?
  "Returns true if the provided value is a string representing a URI with an
  http or https scheme, else returns false."
  [value]
  {:requirement :must-be-an-http-uri-string
   :gen         #(gen-absolute-uri {:schemes #{:http :https}})}
  (sv-utils/exception->false
    (let [uri (uri/uri value)]
      (and (uri/absolute? uri)
        (contains? #{"http" "https"} (:scheme uri))))))
