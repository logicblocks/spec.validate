(ns spec.validate.uri-test
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [clojure.spec.alpha :as spec]
   [clojure.spec.gen.alpha :as gen]

   [lambdaisland.uri :as uri]

   [spec.validate.core :as sv-core]
   [spec.validate.uri :as sv-uri]
   [spec.validate.domain :as sv-domain]
   [spec.validate.unicode :as sv-unicode]

   [spec.validate.test-support.cases :as sv-cases]
   [spec.validate.utils :as sv-utils]))

(deftest http-uri-string?-as-predicate
  (doseq
   [case
    [(sv-cases/true-case "any absolute HTTP URI"
       :samples ["http://example.com/some-path"
                 "https://example.com:9383"
                 "http://example.com/path/to/file.csv"
                 "https://example.com?query=param"])
     (sv-cases/false-case "any absolute non-HTTP URI"
       :samples ["file:///some-file"
                 "file:/path/to/file.csv"
                 "sftp://example.com/some/important/file.html"
                 "ssh://user@host.com"])
     (sv-cases/false-case "any relative URI"
       :samples ["/some-path"
                 "some/important/path"
                 "/path/to/file.csv#fragment"
                 "?query=param"
                 "example.com/some/path"])
     (sv-cases/false-case "a non-string"
       :samples [true false 35.4 #{"GBP" "USD"}])
     (sv-cases/false-case "nil" :sample nil)]]
    (let [{:keys [samples satisfied? title]} case
          pred sv-uri/http-uri-string?]
      (testing (str "for " title)
        (is (every? #(= % satisfied?) (map pred samples))
          (str "unsatisfied for: "
            (into #{} (filter #(not (= (pred %) satisfied?)) samples))))))))

(deftest http-uri-string?-as-requirement
  (is (= :must-be-an-http-uri-string
        (sv-core/pred-requirement
          'spec.validate.uri/http-uri-string?))))

(deftest http-uri-string?-as-generator
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample
              (spec/gen sv-uri/http-uri-string?) 100)))))
  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample
              (spec/gen sv-uri/http-uri-string?) 100)))))
  (testing "generates with only http and https schemes"
    (let [schemes
          (into #{}
            (map #(:scheme (uri/uri %))
              (gen/sample (spec/gen sv-uri/http-uri-string?) 100)))]
      (is (= #{"http" "https"} schemes))))
  (testing "generates a variety of hosts"
    (let [hosts
          (into #{}
            (map #(:host (uri/uri %))
              (gen/sample (spec/gen sv-uri/http-uri-string?) 100)))]
      (is (> (count hosts) 50))))
  (testing "uses the correct character set for hosts"
    (let [hosts
          (into #{}
            (map #(:host (uri/uri %))
              (gen/sample (spec/gen sv-uri/http-uri-string?) 100)))]
      (is (every?
            (fn [host]
              (let [parts (string/split host #"\.")]
                (every?
                  #(sv-utils/re-satisfies?
                     #"([a-z0-9]|[a-z][a-z0-9\-]*[a-z0-9])" %)
                  parts)))
            hosts))))
  (testing "uses IPv4 addresses for some hosts"
    (let [hosts
          (into #{}
            (map #(:host (uri/uri %))
              (gen/sample (spec/gen sv-uri/http-uri-string?) 1000)))
          ipv4-address-hosts
          (filter
            #(sv-utils/re-satisfies? #"^(\d{1,3}\.){3}\d{1,3}$" %)
            hosts)]
      (is (> 1000 (count ipv4-address-hosts) 1))))
  (testing "uses only valid TLDs in domain name hosts"
    (let [hosts
          (into #{}
            (map #(:host (uri/uri %))
              (gen/sample (spec/gen sv-uri/http-uri-string?) 1000)))
          non-ipv4-address-hosts
          (filter
            #(not (sv-utils/re-satisfies? #"^(\d{1,3}\.){3}\d{1,3}$" %))
            hosts)
          non-ipv4-address-hosts
          (map
            (fn [host] (last (string/split host #"\.")))
            non-ipv4-address-hosts)]
      (is (every?
            #(contains? sv-domain/top-level-domain-labels %)
            non-ipv4-address-hosts))))
  (testing "generates including ports sometimes"
    (let [uris
          (gen/sample (spec/gen sv-uri/http-uri-string?) 100)
          uris-with-ports
          (filter #(not (nil? (:port (uri/uri %)))) uris)]
      (is (> 100 (count uris-with-ports) 0))))
  (testing "generates a variety of ports"
    (let [ports
          (into #{}
            (map #(:port (uri/uri %))
              (gen/sample (spec/gen sv-uri/http-uri-string?) 1000)))]
      (is (> (count ports) 20))))
  (testing "generates ports in the correct range"
    (let [ports
          (into #{}
            (map #(:port (uri/uri %))
              (gen/sample (spec/gen sv-uri/http-uri-string?) 1000)))
          non-nil-ports
          (filter #(not (nil? %)) ports)]
      (is (every?
            (fn [port] (>= 65535 (parse-long port) 0))
            non-nil-ports))))
  (testing "generates a variety of paths"
    (let [paths
          (into #{}
            (map #(:path (uri/uri %))
              (gen/sample (spec/gen sv-uri/http-uri-string?) 100)))]
      (is (> (count paths) 50))))
  (testing "generates some empty paths"
    (let [paths
          (into #{}
            (map #(:path (uri/uri %))
              (gen/sample (spec/gen sv-uri/http-uri-string?) 100)))
          empty-paths
          (filter #(nil? %) paths)]
      (is (> (count empty-paths) 0))))
  (testing "generates some root paths"
    (let [paths
          (into #{}
            (map #(:path (uri/uri %))
              (gen/sample (spec/gen sv-uri/http-uri-string?) 100)))
          empty-paths
          (filter #(= "/" %) paths)]
      (is (> (count empty-paths) 0))))
  (testing "generates paths with 1, 2, 3 and 4 segments"
    (let [paths
          (into #{}
            (map #(:path (uri/uri %))
              (gen/sample (spec/gen sv-uri/http-uri-string?) 10000)))
          paths
          (filter #(not (or (nil? %) (re-matches #"^/*$" %))) paths)
          segment-counts
          (into #{} (map #(- (count (string/split % #"/")) 1) paths))]
      (is (= #{1 2 3 4} segment-counts))))
  (testing "generates paths segments using correct encoding"
    (let [paths
          (into #{}
            (map #(:path (uri/uri %))
              (gen/sample (spec/gen sv-uri/http-uri-string?) 100)))
          paths
          (filter #(not (or (nil? %) (= "/" %))) paths)
          segments
          (into #{} (map #(second (string/split % #"/")) paths))
          characters
          (reduce
            (fn [acc segment]
              (if (nil? segment)
                acc
                (let [encoded (map second (re-seq #"(\%..)" segment))
                      remainder (string/replace segment #"\%.." "")
                      plain (map #(Character/toString ^long %)
                              (sv-unicode/unicode-codepoint-seq remainder))]
                  {:encoded (into (:encoded acc) encoded)
                   :plain   (into (:plain acc) plain)})))
            {:encoded #{}
             :plain   #{}}
            segments)]
      (is (empty?
            (set/difference
              (:plain characters)
              (into #{}
                (sv-unicode/unicode-characters
                  "[-._~\\&!$'()*+,;=a-z0-9]")))))
      (is (not (empty? (:encoded characters)))))))
