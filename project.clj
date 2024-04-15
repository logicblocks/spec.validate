(defproject io.logicblocks/spec.validate "0.2.0-RC7"
  :description "A clojure.spec based validation library."
  :url "https://github.com/logicblocks/spec.validate"

  :license {:name "The MIT License"
            :url  "https://opensource.org/licenses/MIT"}

  :plugins [[fipp "0.6.26"]
            [lein-cloverage "1.2.4"]
            [lein-shell "0.5.0"]
            [lein-ancient "0.7.0"]
            [lein-changelog "0.3.2"]
            [lein-cprint "1.3.3"]
            [lein-eftest "0.6.0"]
            [lein-codox "0.10.8"]
            [lein-cljfmt "0.9.2"]
            [lein-kibit "0.1.8"]
            [lein-bikeshed "0.5.2"]
            [jonase/eastwood "1.4.0"]]

  :dependencies [[com.googlecode.libphonenumber/libphonenumber "8.13.12"]
                 [com.ibm.icu/icu4j "72.1"]
                 [lambdaisland/uri "1.19.155"]
                 [com.widdindustries/cljc.java-time "0.1.21"]]

  :profiles
  {:shared
   ^{:pom-scope :test}
   {:dependencies
    [[org.clojure/clojure "1.11.1"]
     [org.clojure/test.check "1.1.1"]
     [org.clojure/data.csv "1.1.0"]

     [vlaaad/reveal "1.3.282"]

     [nrepl "1.1.1"]

     [eftest "0.6.0"]]}

   :dev-specific
   {:source-paths ["dev"]
    :eftest       {:multithread? false}}

   :reveal-specific
   {:repl-options {:nrepl-middleware [vlaaad.reveal.nrepl/middleware]}
    :jvm-opts     ["-Dvlaaad.reveal.prefs={:theme :light}"]}

   :test-specific
   {:eftest {:multithread? false}}

   :dev
   [:shared :dev-specific]

   :reveal
   [:shared :reveal-specific]

   :test
   [:shared :test-specific]

   :prerelease
   {:release-tasks
    [["shell" "git" "diff" "--exit-code"]
     ["change" "version" "leiningen.release/bump-version" "rc"]
     ["change" "version" "leiningen.release/bump-version" "release"]
     ["vcs" "commit" "Pre-release version %s [skip ci]"]
     ["vcs" "tag"]
     ["deploy"]]}

   :release
   {:release-tasks
    [["shell" "git" "diff" "--exit-code"]
     ["change" "version" "leiningen.release/bump-version" "release"]
     ["codox"]
     ["changelog" "release"]
     ["shell" "sed" "-E" "-i.bak" "s/\"[0-9]+\\.[0-9]+\\.[0-9]+\"/\"${:version}\"/g" "README.md"]
     ["shell" "rm" "-f" "README.md.bak"]
     ["shell" "git" "add" "."]
     ["vcs" "commit" "Release version %s [skip ci]"]
     ["vcs" "tag"]
     ["deploy"]
     ["change" "version" "leiningen.release/bump-version" "patch"]
     ["change" "version" "leiningen.release/bump-version" "rc"]
     ["change" "version" "leiningen.release/bump-version" "release"]
     ["vcs" "commit" "Pre-release version %s [skip ci]"]
     ["vcs" "tag"]
     ["vcs" "push"]]}}

  :target-path "target/%s/"

  :cloverage
  {:ns-exclude-regex [#"^user"]}

  :codox
  {:namespaces  [#"^spec\.validate\."]
   :metadata    {:doc/format :markdown}
   :output-path "docs"
   :doc-paths   ["docs"]
   :source-uri  "https://github.com/logicblocks/spec.validate/blob/{version}/{filepath}#L{line}"}

  :cljfmt {:indents ^:replace {#".*" [[:inner 0]]}}

  :eastwood {:config-files ["config/linter.clj"]}

  :bikeshed {:name-collisions false
             :long-lines      false}

  :deploy-repositories
  {"releases"  {:url "https://repo.clojars.org" :creds :gpg}
   "snapshots" {:url "https://repo.clojars.org" :creds :gpg}})
