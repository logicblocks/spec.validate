(ns datatype.uri.gen
  (:require
   [clojure.string :as string]
   [clojure.spec.gen.alpha :as gen]

   [icu4clj.text.unicode-set-patterns :as icu-tusp]

   [lambdaisland.uri :as uri]
   [lambdaisland.uri.normalize :as uri-norm]

   [datatype.uri.scheme :as dt-uri-scheme]
   [datatype.domain.gen :as dt-domain-gen]
   [datatype.network.gen :as dt-network-gen]
   [datatype.string.gen :as dt-string-gen]))

; support username / password
; support fragment
; increase size of query string character set

(def ^:private gen-delims-pattern "[\\:/?#\\[\\]@]")
(def ^:private sub-delims-pattern "[\\&!$'()*+,;=]")

(def ^:private path-non-delims-pattern "[\\:@]")

(def ^:private unreserved-sym-pattern "[-._~]")

(defn gen-path-segment
  ([] (gen-path-segment {}))
  ([{:keys [min-path-segment-length
            max-path-segment-length]}]
   (let [encoded-character-gen
         (dt-string-gen/gen-character-string-unicode
           (icu-tusp/difference gen-delims-pattern path-non-delims-pattern))
         unencoded-character-gen
         (dt-string-gen/gen-character-string-unicode
           (icu-tusp/union unreserved-sym-pattern sub-delims-pattern))
         alpha-digit-gen
         (dt-string-gen/gen-character-string-lowercase-ascii-alphanumerics)]
     (gen/fmap
       string/join
       (gen/vector
         (gen/frequency
           [[1 (gen/fmap uri-norm/percent-encode encoded-character-gen)]
            [1 unencoded-character-gen]
            [10 alpha-digit-gen]])
         (or min-path-segment-length 0)
         (or max-path-segment-length 10))))))

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
         (dt-string-gen/gen-string-lowercase-ascii-alphabetics
           {:min-length (or min-query-param-key-length 1)
            :max-length (or max-query-param-key-length 10)})
         value-gen
         (dt-string-gen/gen-string-lowercase-ascii-alphabetics
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
           (if schemes
             (map name schemes)
             (dt-uri-scheme/uri-schemes)))
         host-gen
         (gen/frequency
           [[(if (true? include-ipv4-addresses?) 1 0)
             (dt-network-gen/gen-ipv4-address-string)]
            [9 (dt-domain-gen/gen-domain-name)]])
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
