(ns datatype.uri.gen-test
  (:require
   [clojure.set :as set]
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as string]
   [clojure.test :refer [deftest testing is]]

   [datatype.support :as dts]
   [datatype.domain.core :as dt-domain]
   [datatype.uri.gen :as dt-uri-gen]
   [datatype.uri.scheme :as dt-uri-scheme]

   [icu4clj.text.unicode-set :as icu-tus]

   [lambdaisland.uri :as uri]))

(deftest gen-absolute-uri
  (testing "does not generate nil"
    (is (every? false?
          (map nil?
            (gen/sample (dt-uri-gen/gen-absolute-uri) 100)))))
  (testing "does not generate empty string"
    (is (every? false?
          (map #(= "" %)
            (gen/sample (dt-uri-gen/gen-absolute-uri) 100)))))
  (testing "generates with only http and https schemes"
    (let [schemes
          (into #{}
            (map #(:scheme (uri/uri %))
              (gen/sample (dt-uri-gen/gen-absolute-uri) 100)))]
      (is (empty?
            (set/difference
              schemes
              (into #{} (dt-uri-scheme/uri-schemes)))))))
  (testing "generates a variety of hosts"
    (let [hosts
          (into #{}
            (map #(:host (uri/uri %))
              (gen/sample (dt-uri-gen/gen-absolute-uri) 100)))]
      (is (> (count hosts) 50))))
  (testing "uses the correct character set for hosts"
    (let [hosts
          (into #{}
            (map #(:host (uri/uri %))
              (gen/sample (dt-uri-gen/gen-absolute-uri) 100)))]
      (is (every?
            (fn [host]
              (let [parts (string/split host #"\.")]
                (every?
                  #(dts/re-satisfies?
                     #"([a-z0-9]|[a-z][a-z0-9\-]*[a-z0-9])" %)
                  parts)))
            hosts))))
  (testing "uses IPv4 addresses for some hosts"
    (let [hosts
          (into #{}
            (map #(:host (uri/uri %))
              (gen/sample (dt-uri-gen/gen-absolute-uri) 1000)))
          ipv4-address-hosts
          (filter
            #(dts/re-satisfies? #"^(\d{1,3}\.){3}\d{1,3}$" %)
            hosts)]
      (is (> 1000 (count ipv4-address-hosts) 1))))
  (testing "uses only valid TLDs in domain name hosts"
    (let [hosts
          (into #{}
            (map #(:host (uri/uri %))
              (gen/sample (dt-uri-gen/gen-absolute-uri) 1000)))
          non-ipv4-address-hosts
          (filter
            #(not (dts/re-satisfies? #"^(\d{1,3}\.){3}\d{1,3}$" %))
            hosts)
          non-ipv4-address-hosts
          (map
            (fn [host] (last (string/split host #"\.")))
            non-ipv4-address-hosts)]
      (is (every?
            #(contains? dt-domain/top-level-domain-labels %)
            non-ipv4-address-hosts))))
  (testing "generates including ports sometimes"
    (let [uris
          (gen/sample (dt-uri-gen/gen-absolute-uri) 100)
          uris-with-ports
          (filter #(not (nil? (:port (uri/uri %)))) uris)]
      (is (> 100 (count uris-with-ports) 0))))
  (testing "generates a variety of ports"
    (let [ports
          (into #{}
            (map #(:port (uri/uri %))
              (gen/sample (dt-uri-gen/gen-absolute-uri) 1000)))]
      (is (> (count ports) 20))))
  (testing "generates ports in the correct range"
    (let [ports
          (into #{}
            (map #(:port (uri/uri %))
              (gen/sample (dt-uri-gen/gen-absolute-uri) 1000)))
          non-nil-ports
          (filter #(not (nil? %)) ports)]
      (is (every?
            (fn [port] (>= 65535 (parse-long port) 0))
            non-nil-ports))))
  (testing "generates a variety of paths"
    (let [paths
          (into #{}
            (map #(:path (uri/uri %))
              (gen/sample (dt-uri-gen/gen-absolute-uri) 100)))]
      (is (> (count paths) 50))))
  (testing "generates some empty paths"
    (let [paths
          (into #{}
            (map #(:path (uri/uri %))
              (gen/sample (dt-uri-gen/gen-absolute-uri) 100)))
          empty-paths
          (filter #(nil? %) paths)]
      (is (> (count empty-paths) 0))))
  (testing "generates some root paths"
    (let [paths
          (into #{}
            (map #(:path (uri/uri %))
              (gen/sample (dt-uri-gen/gen-absolute-uri) 100)))
          empty-paths
          (filter #(= "/" %) paths)]
      (is (> (count empty-paths) 0))))
  (testing "generates paths with 1, 2, 3 and 4 segments"
    (let [paths
          (into #{}
            (map #(:path (uri/uri %))
              (gen/sample (dt-uri-gen/gen-absolute-uri) 10000)))
          paths
          (filter #(not (or (nil? %) (re-matches #"^/*$" %))) paths)
          segment-counts
          (into #{} (map #(- (count (string/split % #"/")) 1) paths))]
      (is (= #{1 2 3 4} segment-counts))))
  (testing "generates paths segments using correct encoding"
    (let [paths
          (into #{}
            (map #(:path (uri/uri %))
              (gen/sample (dt-uri-gen/gen-absolute-uri) 100)))
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
                      plain (map str remainder)]
                  {:encoded (into (:encoded acc) encoded)
                   :plain   (into (:plain acc) plain)})))
            {:encoded #{}
             :plain   #{}}
            segments)]
      (is (empty?
            (set/difference
              (:plain characters)
              (icu-tus/character-set
                "[-._~\\&!$'()*+,;=a-z0-9]"))))
      (is (not (empty? (:encoded characters)))))))
