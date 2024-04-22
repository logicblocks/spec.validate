(ns spec.validate.uri.scheme
  (:require
   [clojure.data.csv :as csv]
   [clojure.string :as string]))

; used to fetch latest URI schemes and convert to below format
(defn fetch-uri-scheme-data []
  (let [uri-schemes-csv-url
        "https://www.iana.org/assignments/uri-schemes/uri-schemes-1.csv"
        template-base-url
        "https://www.iana.org/assignments/uri-schemes"
        rfc-base-url
        "https://www.rfc-editor.org/rfc"
        rfc-match->rfc-url
        (fn [rfc-match]
          (str rfc-base-url "/rfc" (second rfc-match) ".html"))
        rfc-reference-pattern
        #"\[RFC(\d+)\]"
        status-map
        {"Historical"  :historical
         "Provisional" :provisional
         "Permanent"   :permanent}]
    (mapv
      (fn [[uri-scheme
            template
            description
            status
            well-known-uri-support
            reference]]
        (cond->
         {:uri-scheme  uri-scheme
          :description description
          :status      (get status-map status)}

          (contains? uri-scheme "(OBSOLETE)")
          (assoc
            :uri-scheme (string/replace uri-scheme " (OBSOLETE)" "")
            :obsolete? true)

          (not= template "")
          (assoc :template-url (str template-base-url "/" template))

          (re-matches rfc-reference-pattern well-known-uri-support)
          (assoc
            :well-known-uri-support
            (rfc-match->rfc-url
              (re-find rfc-reference-pattern well-known-uri-support)))

          (re-matches rfc-reference-pattern reference)
          (assoc
            :references
            (mapv rfc-match->rfc-url
              (re-seq rfc-reference-pattern reference)))))
      (drop 1                                               ; drop header row
        (csv/read-csv (slurp uri-schemes-csv-url))))))

; taken from: https://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml
(def uri-scheme-data
  [{:uri-scheme  "aaa",
    :description "Diameter Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc6733.html"]}
   {:uri-scheme  "aaas",
    :description "Diameter Protocol with Secure Transport",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc6733.html"]}
   {:uri-scheme  "about",
    :description "about",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc6694.html"]}
   {:uri-scheme  "acap",
    :description "application configuration access protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc2244.html"]}
   {:uri-scheme  "acct",
    :description "acct",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc7565.html"]}
   {:uri-scheme   "acd",
    :description  "acd",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/acd"}
   {:uri-scheme   "acr",
    :description  "acr",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/acr"}
   {:uri-scheme   "adiumxtra",
    :description  "adiumxtra",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/adiumxtra"}
   {:uri-scheme   "adt",
    :description  "adt",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/adt"}
   {:uri-scheme   "afp",
    :description  "afp",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/afp"}
   {:uri-scheme  "afs",
    :description "Andrew File System global file names",
    :status      :provisional,
    :references  ["https://www.rfc-editor.org/rfc/rfc1738.html"]}
   {:uri-scheme   "aim",
    :description  "aim",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/aim"}
   {:uri-scheme   "amss",
    :description  "amss",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/amss"}
   {:uri-scheme   "android",
    :description  "android",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/android"}
   {:uri-scheme   "appdata",
    :description  "appdata",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/appdata"}
   {:uri-scheme   "apt",
    :description  "apt",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/apt"}
   {:uri-scheme   "ar",
    :description  "ar",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ar"}
   {:uri-scheme   "ark",
    :description  "ark",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ark"}
   {:uri-scheme   "at",
    :description  "at (see [reviewer notes])",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/at"}
   {:uri-scheme  "attachment",
    :description "attachment",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/attachment"}
   {:uri-scheme   "aw",
    :description  "aw",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/aw"}
   {:uri-scheme   "barion",
    :description  "barion",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/barion"}
   {:uri-scheme   "bb",
    :description  "bb",
    :status       :historical,
    :template-url "https://www.iana.org/assignments/uri-schemes/historic/bb"}
   {:uri-scheme   "beshare",
    :description  "beshare",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/beshare"}
   {:uri-scheme   "bitcoin",
    :description  "bitcoin",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/bitcoin"}
   {:uri-scheme  "bitcoincash",
    :description "bitcoincash",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/bitcoincash"}
   {:uri-scheme   "blob",
    :description  "blob",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/blob"}
   {:uri-scheme   "bolo",
    :description  "bolo",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/bolo"}
   {:uri-scheme   "brid",
    :description  "brid",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/brid"}
   {:uri-scheme  "browserext",
    :description "browserext",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/browserext"}
   {:uri-scheme   "cabal",
    :description  "cabal",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/cabal"}
   {:uri-scheme  "calculator",
    :description "calculator",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/calculator"}
   {:uri-scheme   "callto",
    :description  "callto",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/callto"}
   {:uri-scheme  "cap",
    :description "Calendar Access Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4324.html"]}
   {:uri-scheme   "cast",
    :description  "cast",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/cast"}
   {:uri-scheme   "casts",
    :description  "casts",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/casts"}
   {:uri-scheme   "chrome",
    :description  "chrome",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/chrome"}
   {:uri-scheme  "chrome-extension",
    :description "chrome-extension",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/chrome-extension"}
   {:uri-scheme  "cid",
    :description "content identifier",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc2392.html"]}
   {:uri-scheme             "coap",
    :description            "coap",
    :status                 :permanent,
    :well-known-uri-support "https://www.rfc-editor.org/rfc/rfc7252.html",
    :references             ["https://www.rfc-editor.org/rfc/rfc7252.html"]}
   {:uri-scheme             "coap+tcp",
    :description            "coap+tcp (see [reviewer notes])",
    :status                 :permanent,
    :well-known-uri-support "https://www.rfc-editor.org/rfc/rfc8323.html",
    :references             ["https://www.rfc-editor.org/rfc/rfc8323.html"]}
   {:uri-scheme             "coap+ws",
    :description            "coap+ws (see [reviewer notes])",
    :status                 :permanent,
    :well-known-uri-support "https://www.rfc-editor.org/rfc/rfc8323.html",
    :references             ["https://www.rfc-editor.org/rfc/rfc8323.html"]}
   {:uri-scheme             "coaps",
    :description            "coaps",
    :status                 :permanent,
    :well-known-uri-support "https://www.rfc-editor.org/rfc/rfc7252.html",
    :references             ["https://www.rfc-editor.org/rfc/rfc7252.html"]}
   {:uri-scheme             "coaps+tcp",
    :description            "coaps+tcp (see [reviewer notes])",
    :status                 :permanent,
    :well-known-uri-support "https://www.rfc-editor.org/rfc/rfc8323.html",
    :references             ["https://www.rfc-editor.org/rfc/rfc8323.html"]}
   {:uri-scheme             "coaps+ws",
    :description            "coaps+ws (see [reviewer notes])",
    :status                 :permanent,
    :well-known-uri-support "https://www.rfc-editor.org/rfc/rfc8323.html",
    :references             ["https://www.rfc-editor.org/rfc/rfc8323.html"]}
   {:uri-scheme  "com-eventbrite-attendee",
    :description "com-eventbrite-attendee",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/com-eventbrite-attendee"}
   {:uri-scheme   "content",
    :description  "content",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/content"}
   {:uri-scheme  "content-type",
    :description "content-type",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/content-type"}
   {:uri-scheme  "crid",
    :description "TV-Anytime Content Reference Identifier",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4078.html"]}
   {:uri-scheme   "cstr",
    :description  "cstr",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/cstr"}
   {:uri-scheme   "cvs",
    :description  "cvs",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/cvs"}
   {:uri-scheme   "dab",
    :description  "dab",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/dab"}
   {:uri-scheme   "dat",
    :description  "dat",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/dat"}
   {:uri-scheme  "data",
    :description "data",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc2397.html"]}
   {:uri-scheme  "dav",
    :description "dav",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4918.html"]}
   {:uri-scheme   "dhttp",
    :description  "dhttp (see [reviewer notes])",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/dhttp"}
   {:uri-scheme   "diaspora",
    :description  "diaspora",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/diaspora"}
   {:uri-scheme  "dict",
    :description "dictionary service protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc2229.html"]}
   {:uri-scheme   "did",
    :description  "did",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/did"}
   {:uri-scheme   "dis",
    :description  "dis",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/dis"}
   {:uri-scheme  "dlna-playcontainer",
    :description "dlna-playcontainer",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/dlna-playcontainer"}
   {:uri-scheme  "dlna-playsingle",
    :description "dlna-playsingle",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/dlna-playsingle"}
   {:uri-scheme  "dns",
    :description "Domain Name System",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4501.html"]}
   {:uri-scheme   "dntp",
    :description  "dntp",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/dntp"}
   {:uri-scheme   "doi",
    :description  "doi",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/doi"}
   {:uri-scheme   "dpp",
    :description  "dpp",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/dpp"}
   {:uri-scheme   "drm",
    :description  "drm",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/drm"}
   {:uri-scheme   "drop",
    :description  "drop",
    :status       :historical,
    :template-url "https://www.iana.org/assignments/uri-schemes/historic/drop"}
   {:uri-scheme   "dtmi",
    :description  "dtmi",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/dtmi"}
   {:uri-scheme  "dtn",
    :description "DTNRG research and development",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc9171.html"]}
   {:uri-scheme "dvb", :description "dvb", :status :provisional}
   {:uri-scheme   "dvx",
    :description  "dvx",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/dvx"}
   {:uri-scheme   "dweb",
    :description  "dweb",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/dweb"}
   {:uri-scheme   "ed2k",
    :description  "ed2k",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ed2k"}
   {:uri-scheme   "eid",
    :description  "eid",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/eid"}
   {:uri-scheme   "elsi",
    :description  "elsi",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/elsi"}
   {:uri-scheme   "embedded",
    :description  "embedded",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/embedded"}
   {:uri-scheme   "ens",
    :description  "ens",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ens"}
   {:uri-scheme   "ethereum",
    :description  "ethereum",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ethereum"}
   {:uri-scheme  "example",
    :description "example",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc7595.html"]}
   {:uri-scheme   "facetime",
    :description  "facetime",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/facetime"}
   {:uri-scheme "fax", :description "fax", :status :historical}
   {:uri-scheme   "feed",
    :description  "feed",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/feed"}
   {:uri-scheme   "feedready",
    :description  "feedready",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/feedready"}
   {:uri-scheme   "fido",
    :description  "fido",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/fido"}
   {:uri-scheme  "file",
    :description "Host-specific file names",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc8089.html"]}
   {:uri-scheme  "filesystem",
    :description "filesystem",
    :status      :historical,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/historic/filesystem"}
   {:uri-scheme   "finger",
    :description  "finger",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/finger"}
   {:uri-scheme   "first-run-pen-experience",
    :description  "first-run-pen-experience",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/prov/"
                    "first-run-pen-experience")}
   {:uri-scheme   "fish",
    :description  "fish",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/fish"}
   {:uri-scheme   "fm",
    :description  "fm",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/fm"}
   {:uri-scheme  "ftp",
    :description "File Transfer Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc1738.html"]}
   {:uri-scheme  "fuchsia-pkg",
    :description "fuchsia-pkg",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/fuchsia-pkg"}
   {:uri-scheme  "geo",
    :description "Geographic Locations",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc5870.html"]}
   {:uri-scheme   "gg",
    :description  "gg",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/gg"}
   {:uri-scheme   "git",
    :description  "git",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/git"}
   {:uri-scheme   "gitoid",
    :description  "gitoid",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/gitoid"}
   {:uri-scheme  "gizmoproject",
    :description "gizmoproject",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/gizmoproject"}
   {:uri-scheme  "go",
    :description "go",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3368.html"]}
   {:uri-scheme  "gopher",
    :description "The Gopher Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4266.html"]}
   {:uri-scheme   "graph",
    :description  "graph",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/graph"}
   {:uri-scheme   "grd",
    :description  "grd",
    :status       :historical,
    :template-url "https://www.iana.org/assignments/uri-schemes/historic/grd"}
   {:uri-scheme   "gtalk",
    :description  "gtalk",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/gtalk"}
   {:uri-scheme  "h323",
    :description "H.323",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3508.html"]}
   {:uri-scheme  "ham",
    :description "ham",
    :status      :provisional,
    :references  ["https://www.rfc-editor.org/rfc/rfc7046.html"]}
   {:uri-scheme   "hcap",
    :description  "hcap",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/hcap"}
   {:uri-scheme   "hcp",
    :description  "hcp",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/hcp"}
   {:uri-scheme             "http",
    :description            "Hypertext Transfer Protocol",
    :status                 :permanent,
    :well-known-uri-support "https://www.rfc-editor.org/rfc/rfc8615.html"}
   {:uri-scheme             "https",
    :description            "Hypertext Transfer Protocol Secure",
    :status                 :permanent,
    :well-known-uri-support "https://www.rfc-editor.org/rfc/rfc8615.html"}
   {:uri-scheme   "hxxp",
    :description  "hxxp",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/hxxp"}
   {:uri-scheme   "hxxps",
    :description  "hxxps",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/hxxps"}
   {:uri-scheme   "hydrazone",
    :description  "hydrazone",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/hydrazone"}
   {:uri-scheme   "hyper",
    :description  "hyper",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/hyper"}
   {:uri-scheme  "iax",
    :description "Inter-Asterisk eXchange Version 2",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc5456.html"]}
   {:uri-scheme  "icap",
    :description "Internet Content Adaptation Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3507.html"]}
   {:uri-scheme "icon", :description "icon", :status :provisional}
   {:uri-scheme  "im",
    :description "Instant Messaging",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3860.html"]}
   {:uri-scheme  "imap",
    :description "internet message access protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc5092.html"]}
   {:uri-scheme  "info",
    :description (str "Information Assets with Identifiers in Public "
                   "Namespaces. [RFC4452] (section 3) defines an \"info\" "
                   "registry of public namespaces, which is maintained by "
                   "NISO and can be accessed from [http://info-uri.info/]."),
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4452.html"]}
   {:uri-scheme   "iotdisco",
    :description  "iotdisco",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/iotdisco"}
   {:uri-scheme   "ipfs",
    :description  "ipfs",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ipfs"}
   {:uri-scheme  "ipn",
    :description "ipn",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc9171.html"]}
   {:uri-scheme   "ipns",
    :description  "ipns",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ipns"}
   {:uri-scheme  "ipp",
    :description "Internet Printing Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3510.html"]}
   {:uri-scheme  "ipps",
    :description "Internet Printing Protocol over HTTPS",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc7472.html"]}
   {:uri-scheme   "irc",
    :description  "irc",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/irc"}
   {:uri-scheme   "irc6",
    :description  "irc6",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/irc6"}
   {:uri-scheme   "ircs",
    :description  "ircs",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ircs"}
   {:uri-scheme  "iris",
    :description "Internet Registry Information Service",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3981.html"]}
   {:uri-scheme  "iris.beep",
    :description "iris.beep",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3983.html"]}
   {:uri-scheme  "iris.lwz",
    :description "iris.lwz",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4993.html"]}
   {:uri-scheme  "iris.xpc",
    :description "iris.xpc",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4992.html"]}
   {:uri-scheme  "iris.xpcs",
    :description "iris.xpcs",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4992.html"]}
   {:uri-scheme   "isostore",
    :description  "isostore",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/isostore"}
   {:uri-scheme   "itms",
    :description  "itms",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/itms"}
   {:uri-scheme   "jabber",
    :description  "jabber",
    :status       :permanent,
    :template-url "https://www.iana.org/assignments/uri-schemes/perm/jabber"}
   {:uri-scheme   "jar",
    :description  "jar",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/jar"}
   {:uri-scheme  "jms",
    :description "Java Message Service",
    :status      :provisional,
    :references  ["https://www.rfc-editor.org/rfc/rfc6167.html"]}
   {:uri-scheme   "keyparc",
    :description  "keyparc",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/keyparc"}
   {:uri-scheme   "lastfm",
    :description  "lastfm",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/lastfm"}
   {:uri-scheme   "lbry",
    :description  "lbry",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/lbry"}
   {:uri-scheme  "ldap",
    :description "Lightweight Directory Access Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4516.html"]}
   {:uri-scheme   "ldaps",
    :description  "ldaps",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ldaps"}
   {:uri-scheme  "leaptofrogans",
    :description "leaptofrogans",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc8589.html"]}
   {:uri-scheme   "lid",
    :description  "lid",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/lid"}
   {:uri-scheme   "lorawan",
    :description  "lorawan",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/lorawan"}
   {:uri-scheme   "lpa",
    :description  "lpa",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/lpa"}
   {:uri-scheme   "lvlt",
    :description  "lvlt",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/lvlt"}
   {:uri-scheme   "machineProvisioningProgressReporter",
    :description  "Windows Autopilot Modern Device Management status updates",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/machineProvisioningProgressReporter")}
   {:uri-scheme   "magnet",
    :description  "magnet",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/magnet"}
   {:uri-scheme  "mailserver",
    :description "Access to data available from mail servers",
    :status      :historical,
    :references  ["https://www.rfc-editor.org/rfc/rfc6196.html"]}
   {:uri-scheme  "mailto",
    :description "Electronic mail address",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc6068.html"]}
   {:uri-scheme   "maps",
    :description  "maps",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/maps"}
   {:uri-scheme   "market",
    :description  "market",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/market"}
   {:uri-scheme   "matrix",
    :description  "matrix",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/matrix"}
   {:uri-scheme   "message",
    :description  "message",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/message"}
   {:uri-scheme   "microsoft.windows.camera",
    :description  "microsoft.windows.camera",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/microsoft.windows.camera")}
   {:uri-scheme   "microsoft.windows.camera.multipicker",
    :description  "microsoft.windows.camera.multipicker",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/microsoft.windows.camera.multipicker")}
   {:uri-scheme   "microsoft.windows.camera.picker",
    :description  "microsoft.windows.camera.picker",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/microsoft.windows.camera.picker")}
   {:uri-scheme  "mid",
    :description "message identifier",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc2392.html"]}
   {:uri-scheme   "mms",
    :description  "mms",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/mms"}
   {:uri-scheme "modem", :description "modem", :status :historical}
   {:uri-scheme   "mongodb",
    :description  "mongodb",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/mongodb"}
   {:uri-scheme   "moz",
    :description  "moz",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/moz"}
   {:uri-scheme   "ms-access",
    :description  "ms-access",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ms-access"}
   {:uri-scheme  "ms-appinstaller",
    :description "ms-appinstaller",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-appinstaller"}
   {:uri-scheme  "ms-browser-extension",
    :description "ms-browser-extension",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-browser-extension"}
   {:uri-scheme  "ms-calculator",
    :description "ms-calculator",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-calculator"}
   {:uri-scheme  "ms-drive-to",
    :description "ms-drive-to",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-drive-to"}
   {:uri-scheme  "ms-enrollment",
    :description "ms-enrollment",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-enrollment"}
   {:uri-scheme   "ms-excel",
    :description  "ms-excel",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ms-excel"}
   {:uri-scheme  "ms-eyecontrolspeech",
    :description "ms-eyecontrolspeech",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-eyecontrolspeech"}
   {:uri-scheme  "ms-gamebarservices",
    :description "ms-gamebarservices",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-gamebarservices"}
   {:uri-scheme  "ms-gamingoverlay",
    :description "ms-gamingoverlay",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-gamingoverlay"}
   {:uri-scheme  "ms-getoffice",
    :description "ms-getoffice",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-getoffice"}
   {:uri-scheme   "ms-help",
    :description  "ms-help",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ms-help"}
   {:uri-scheme  "ms-infopath",
    :description "ms-infopath",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-infopath"}
   {:uri-scheme  "ms-inputapp",
    :description "ms-inputapp",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-inputapp"}
   {:uri-scheme  "ms-launchremotedesktop",
    :description "ms-launchremotedesktop",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-launchremotedesktop"}
   {:uri-scheme   "ms-lockscreencomponent-config",
    :description  "ms-lockscreencomponent-config",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-lockscreencomponent-config")}
   {:uri-scheme  "ms-media-stream-id",
    :description "ms-media-stream-id",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-media-stream-id"}
   {:uri-scheme  "ms-meetnow",
    :description "ms-meetnow",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-meetnow"}
   {:uri-scheme  "ms-mixedrealitycapture",
    :description "ms-mixedrealitycapture",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-mixedrealitycapture"}
   {:uri-scheme  "ms-mobileplans",
    :description "ms-mobileplans",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-mobileplans"}
   {:uri-scheme  "ms-newsandinterests",
    :description "ms-newsandinterests",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-newsandinterests"}
   {:uri-scheme  "ms-officeapp",
    :description "ms-officeapp",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-officeapp"}
   {:uri-scheme  "ms-people",
    :description "ms-people",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-people"}
   {:uri-scheme  "ms-project",
    :description "ms-project",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-project"}
   {:uri-scheme  "ms-powerpoint",
    :description "ms-powerpoint",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-powerpoint"}
   {:uri-scheme  "ms-publisher",
    :description "ms-publisher",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-publisher"}
   {:uri-scheme  "ms-remotedesktop",
    :description "ms-remotedesktop",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-remotedesktop"}
   {:uri-scheme  "ms-remotedesktop-launch",
    :description "ms-remotedesktop-launch",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-remotedesktop-launch"}
   {:uri-scheme  "ms-restoretabcompanion",
    :description "ms-restoretabcompanion",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-restoretabcompanion"}
   {:uri-scheme  "ms-screenclip",
    :description "ms-screenclip",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-screenclip"}
   {:uri-scheme  "ms-screensketch",
    :description "ms-screensketch",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-screensketch"}
   {:uri-scheme   "ms-search",
    :description  "ms-search",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ms-search"}
   {:uri-scheme  "ms-search-repair",
    :description "ms-search-repair",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-search-repair"}
   {:uri-scheme   "ms-secondary-screen-controller",
    :description  "ms-secondary-screen-controller",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-secondary-screen-controller")}
   {:uri-scheme   "ms-secondary-screen-setup",
    :description  "ms-secondary-screen-setup",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-secondary-screen-setup")}
   {:uri-scheme  "ms-settings",
    :description "ms-settings",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-settings"}
   {:uri-scheme   "ms-settings-airplanemode",
    :description  "ms-settings-airplanemode",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-airplanemode")}
   {:uri-scheme   "ms-settings-bluetooth",
    :description  "ms-settings-bluetooth",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-bluetooth")}
   {:uri-scheme   "ms-settings-camera",
    :description  "ms-settings-camera",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-camera")}
   {:uri-scheme  "ms-settings-cellular",
    :description "ms-settings-cellular",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-settings-cellular"}
   {:uri-scheme   "ms-settings-cloudstorage",
    :description  "ms-settings-cloudstorage",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-cloudstorage")}
   {:uri-scheme   "ms-settings-connectabledevices",
    :description  "ms-settings-connectabledevices",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-connectabledevices")}
   {:uri-scheme   "ms-settings-displays-topology",
    :description  "ms-settings-displays-topology",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-displays-topology")}
   {:uri-scheme   "ms-settings-emailandaccounts",
    :description  "ms-settings-emailandaccounts",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-emailandaccounts")}
   {:uri-scheme  "ms-settings-language",
    :description "ms-settings-language",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-settings-language"}
   {:uri-scheme  "ms-settings-location",
    :description "ms-settings-location",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-settings-location"}
   {:uri-scheme  "ms-settings-lock",
    :description "ms-settings-lock",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-settings-lock"}
   {:uri-scheme   "ms-settings-nfctransactions",
    :description  "ms-settings-nfctransactions",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-nfctransactions")}
   {:uri-scheme   "ms-settings-notifications",
    :description  "ms-settings-notifications",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-notifications")}
   {:uri-scheme   "ms-settings-power",
    :description  "ms-settings-power",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-power")}
   {:uri-scheme   "ms-settings-privacy",
    :description  "ms-settings-privacy",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-privacy")}
   {:uri-scheme   "ms-settings-proximity",
    :description  "ms-settings-proximity",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-proximity")}
   {:uri-scheme   "ms-settings-screenrotation",
    :description  "ms-settings-screenrotation",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-screenrotation")}
   {:uri-scheme   "ms-settings-wifi",
    :description  "ms-settings-wifi",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-wifi")}
   {:uri-scheme   "ms-settings-workplace",
    :description  "ms-settings-workplace",
    :status       :provisional,
    :template-url (str "https://www.iana.org/assignments/uri-schemes/"
                    "prov/ms-settings-workplace")}
   {:uri-scheme   "ms-spd",
    :description  "ms-spd",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ms-spd"}
   {:uri-scheme  "ms-stickers",
    :description "ms-stickers",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-stickers"}
   {:uri-scheme  "ms-sttoverlay",
    :description "ms-sttoverlay",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-sttoverlay"}
   {:uri-scheme  "ms-transit-to",
    :description "ms-transit-to",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-transit-to"}
   {:uri-scheme  "ms-useractivityset",
    :description "ms-useractivityset",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-useractivityset"}
   {:uri-scheme  "ms-virtualtouchpad",
    :description "ms-virtualtouchpad",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-virtualtouchpad"}
   {:uri-scheme   "ms-visio",
    :description  "ms-visio",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ms-visio"}
   {:uri-scheme  "ms-walk-to",
    :description "ms-walk-to",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-walk-to"}
   {:uri-scheme  "ms-whiteboard",
    :description "ms-whiteboard",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-whiteboard"}
   {:uri-scheme  "ms-whiteboard-cmd",
    :description "ms-whiteboard-cmd",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/ms-whiteboard-cmd"}
   {:uri-scheme   "ms-word",
    :description  "ms-word",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ms-word"}
   {:uri-scheme   "msnim",
    :description  "msnim",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/msnim"}
   {:uri-scheme  "msrp",
    :description "Message Session Relay Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4975.html"]}
   {:uri-scheme  "msrps",
    :description "Message Session Relay Protocol Secure",
    :status      :permanent}
   {:uri-scheme   "mss",
    :description  "mss",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/mss"}
   {:uri-scheme   "mt",
    :description  (str "Matter protocol on-boarding payloads that are encoded "
                    "for use in QR Codes and/or NFC Tags"),
    :status       :permanent,
    :template-url "https://www.iana.org/assignments/uri-schemes/perm/mt"}
   {:uri-scheme  "mtqp",
    :description "Message Tracking Query Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3887.html"]}
   {:uri-scheme   "mumble",
    :description  "mumble",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/mumble"}
   {:uri-scheme  "mupdate",
    :description "Mailbox Update (MUPDATE) Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3656.html"]}
   {:uri-scheme   "mvn",
    :description  "mvn",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/mvn"}
   {:uri-scheme   "mvrp",
    :description  "mvrp\n(see [reviewer notes])",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/mvrp"}
   {:uri-scheme   "mvrps",
    :description  "mvrps\n(see [reviewer notes])",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/mvrps"}
   {:uri-scheme  "news",
    :description "USENET news",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc5538.html"]}
   {:uri-scheme  "nfs",
    :description "network file system protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc2224.html"]}
   {:uri-scheme  "ni",
    :description "ni",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc6920.html"]}
   {:uri-scheme  "nih",
    :description "nih",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc6920.html"]}
   {:uri-scheme  "nntp",
    :description "USENET news using NNTP access",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc5538.html"]}
   {:uri-scheme   "notes",
    :description  "notes",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/notes"}
   {:uri-scheme   "num",
    :description  "Namespace Utility Modules",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/num"}
   {:uri-scheme   "ocf",
    :description  "ocf",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ocf"}
   {:uri-scheme   "oid",
    :description  "oid",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/oid"}
   {:uri-scheme   "onenote",
    :description  "onenote",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/onenote"}
   {:uri-scheme  "onenote-cmd",
    :description "onenote-cmd",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/onenote-cmd"}
   {:uri-scheme  "opaquelocktoken",
    :description "opaquelocktokent",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4918.html"]}
   {:uri-scheme   "openid",
    :description  "OpenID Connect",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/openid"}
   {:uri-scheme  "openpgp4fpr",
    :description "openpgp4fpr",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/openpgp4fpr"}
   {:uri-scheme   "otpauth",
    :description  "otpauth",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/otpauth"}
   {:uri-scheme   "p1",
    :description  "p1",
    :status       :historical,
    :template-url "https://www.iana.org/assignments/uri-schemes/historic/p1"}
   {:uri-scheme   "pack",
    :description  "pack",
    :status       :historical,
    :template-url "https://www.iana.org/assignments/uri-schemes/historic/pack"}
   {:uri-scheme   "palm",
    :description  "palm",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/palm"}
   {:uri-scheme   "paparazzi",
    :description  "paparazzi",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/paparazzi"}
   {:uri-scheme  "payment",
    :description "payment",
    :status      :historical,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/historic/payment"}
   {:uri-scheme   "payto",
    :description  "payto",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/payto",
    :references   ["https://www.rfc-editor.org/rfc/rfc8905.html"]}
   {:uri-scheme  "pkcs11",
    :description "PKCS#11",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc7512.html"]}
   {:uri-scheme   "platform",
    :description  "platform",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/platform"}
   {:uri-scheme  "pop",
    :description "Post Office Protocol v3",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc2384.html"]}
   {:uri-scheme  "pres",
    :description "Presence",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3859.html"]}
   {:uri-scheme  "prospero",
    :description "Prospero Directory Service",
    :status      :historical,
    :references  ["https://www.rfc-editor.org/rfc/rfc4157.html"]}
   {:uri-scheme   "proxy",
    :description  "proxy",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/proxy"}
   {:uri-scheme   "pwid",
    :description  "pwid",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/pwid"}
   {:uri-scheme   "psyc",
    :description  "psyc",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/psyc"}
   {:uri-scheme   "pttp",
    :description  "pttp",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/pttp"}
   {:uri-scheme   "qb",
    :description  "qb",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/qb"}
   {:uri-scheme   "query",
    :description  "query",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/query"}
   {:uri-scheme  "quic-transport",
    :description "quic-transport",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/quic-transport"}
   {:uri-scheme   "redis",
    :description  "redis",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/redis"}
   {:uri-scheme   "rediss",
    :description  "rediss",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/rediss"}
   {:uri-scheme  "reload",
    :description "reload",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc6940.html"]}
   {:uri-scheme   "res",
    :description  "res",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/res"}
   {:uri-scheme   "resource",
    :description  "resource",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/resource"}
   {:uri-scheme   "rmi",
    :description  "rmi",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/rmi"}
   {:uri-scheme  "rsync",
    :description "rsync",
    :status      :provisional,
    :references  ["https://www.rfc-editor.org/rfc/rfc5781.html"]}
   {:uri-scheme   "rtmfp",
    :description  "rtmfp",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/rtmfp",
    :references   ["https://www.rfc-editor.org/rfc/rfc7425.html"]}
   {:uri-scheme   "rtmp",
    :description  "rtmp",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/rtmp"}
   {:uri-scheme  "rtsp",
    :description "Real-Time Streaming Protocol (RTSP)",
    :status      :permanent}
   {:uri-scheme  "rtsps",
    :description "Real-Time Streaming Protocol (RTSP) over TLS",
    :status      :permanent}
   {:uri-scheme  "rtspu",
    :description (str "Real-Time Streaming Protocol (RTSP) over unreliable "
                   "datagram transport"),
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc2326.html"]}
   {:uri-scheme   "sarif",
    :description  "sarif",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/sarif"}
   {:uri-scheme  "secondlife",
    :description "query",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/secondlife"}
   {:uri-scheme  "secret-token",
    :description "secret-token",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/secret-token",
    :references  ["https://www.rfc-editor.org/rfc/rfc8959.html"]}
   {:uri-scheme  "service",
    :description "service location",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc2609.html"]}
   {:uri-scheme  "session",
    :description "session",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc6787.html"]}
   {:uri-scheme   "sftp",
    :description  "query",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/sftp"}
   {:uri-scheme   "sgn",
    :description  "sgn",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/sgn"}
   {:uri-scheme   "shc",
    :description  "shc",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/shc"}
   {:uri-scheme  "shttp",
    :description "Secure Hypertext Transfer Protocol",
    :status      :permanent
    :obsolete?   true}
   {:uri-scheme  "sieve",
    :description "ManageSieve Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc5804.html"]}
   {:uri-scheme  "simpleledger",
    :description "simpleledger",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/simpleledger"}
   {:uri-scheme   "simplex",
    :description  "simplex",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/simplex"}
   {:uri-scheme  "sip",
    :description "session initiation protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3261.html"]}
   {:uri-scheme  "sips",
    :description "secure session initiation protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3261.html"]}
   {:uri-scheme   "skype",
    :description  "skype",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/skype"}
   {:uri-scheme   "smb",
    :description  "smb",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/smb"}
   {:uri-scheme   "smp",
    :description  "smp",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/smp"}
   {:uri-scheme  "sms",
    :description "Short Message Service",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc5724.html"]}
   {:uri-scheme   "smtp",
    :description  "smtp",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/smtp"}
   {:uri-scheme  "snews",
    :description "NNTP over SSL/TLS",
    :status      :historical,
    :references  ["https://www.rfc-editor.org/rfc/rfc5538.html"]}
   {:uri-scheme  "snmp",
    :description "Simple Network Management Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4088.html"]}
   {:uri-scheme  "soap.beep",
    :description "soap.beep",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4227.html"]}
   {:uri-scheme  "soap.beeps",
    :description "soap.beeps",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4227.html"]}
   {:uri-scheme   "soldat",
    :description  "soldat",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/soldat"}
   {:uri-scheme   "spiffe",
    :description  "spiffe",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/spiffe"}
   {:uri-scheme   "spotify",
    :description  "spotify",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/spotify"}
   {:uri-scheme   "ssb",
    :description  "ssb",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ssb"}
   {:uri-scheme   "ssh",
    :description  "ssh",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ssh"}
   {:uri-scheme   "starknet",
    :description  "starknet",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/starknet"}
   {:uri-scheme   "steam",
    :description  "steam",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/steam"}
   {:uri-scheme  "stun",
    :description "stun",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc7064.html"]}
   {:uri-scheme  "stuns",
    :description "stuns",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc7064.html"]}
   {:uri-scheme   "submit",
    :description  "submit",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/submit"}
   {:uri-scheme   "svn",
    :description  "svn",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/svn"}
   {:uri-scheme   "swh",
    :description  "swh",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/swh"}
   {:uri-scheme   "swid",
    :description  "swid (see [reviewer notes])",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/swid"}
   {:uri-scheme   "swidpath",
    :description  "swidpath (see [reviewer notes])",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/swidpath"}
   {:uri-scheme  "tag",
    :description "tag",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4151.html"]}
   {:uri-scheme   "taler",
    :description  "taler",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/taler"}
   {:uri-scheme   "teamspeak",
    :description  "teamspeak",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/teamspeak"}
   {:uri-scheme "tel", :description "telephone", :status :permanent}
   {:uri-scheme   "teliaeid",
    :description  "teliaeid",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/teliaeid"}
   {:uri-scheme  "telnet",
    :description "Reference to interactive sessions",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc4248.html"]}
   {:uri-scheme  "tftp",
    :description "Trivial File Transfer Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3617.html"]}
   {:uri-scheme   "things",
    :description  "things",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/things"}
   {:uri-scheme  "thismessage",
    :description "multipart/related relative reference resolution",
    :status      :permanent,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/perm/thismessage",
    :references  ["https://www.rfc-editor.org/rfc/rfc2557.html"]}
   {:uri-scheme  "tip",
    :description "Transaction Internet Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc2371.html"]}
   {:uri-scheme  "tn3270",
    :description "Interactive 3270 emulation sessions",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc6270.html"]}
   {:uri-scheme   "tool",
    :description  "tool",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/tool"}
   {:uri-scheme  "turn",
    :description "turn",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc7065.html"]}
   {:uri-scheme  "turns",
    :description "turns",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc7065.html"]}
   {:uri-scheme  "tv",
    :description "TV Broadcasts",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc2838.html"]}
   {:uri-scheme   "udp",
    :description  "udp",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/udp"}
   {:uri-scheme   "unreal",
    :description  "unreal",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/unreal"}
   {:uri-scheme   "upt",
    :description  "upt",
    :status       :historical,
    :template-url "https://www.iana.org/assignments/uri-schemes/historic/upt"}
   {:uri-scheme  "urn"
    :description "Uniform Resource Names"
    :status      :permanent}
   {:uri-scheme   "ut2004",
    :description  "ut2004",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ut2004"}
   {:uri-scheme  "uuid-in-package",
    :description "uuid-in-package",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/uuid-in-package"}
   {:uri-scheme   "v-event",
    :description  "v-event",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/v-event"}
   {:uri-scheme  "vemmi",
    :description "versatile multimedia interface",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc2122.html"]}
   {:uri-scheme   "ventrilo",
    :description  "ventrilo",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ventrilo"}
   {:uri-scheme   "ves",
    :description  "ves",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ves"}
   {:uri-scheme  "videotex",
    :description "videotex",
    :status      :historical,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/historic/videotex"}
   {:uri-scheme  "vnc",
    :description "Remote Framebuffer Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc7869.html"]}
   {:uri-scheme  "view-source",
    :description "view-source",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/view-source"}
   {:uri-scheme   "vscode",
    :description  "vscode",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/vscode"}
   {:uri-scheme  "vscode-insiders",
    :description "vscode-insiders",
    :status      :provisional,
    :template-url
    "https://www.iana.org/assignments/uri-schemes/prov/vscode-insiders"}
   {:uri-scheme   "vsls",
    :description  "vsls",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/vsls"}
   {:uri-scheme   "w3",
    :description  "w3 (see [reviewer notes])",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/w3"}
   {:uri-scheme  "wais",
    :description "Wide Area Information Servers",
    :status      :historical,
    :references  ["https://www.rfc-editor.org/rfc/rfc4156.html"]}
   {:uri-scheme   "web3",
    :description  "web3",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/web3"}
   {:uri-scheme   "wcr",
    :description  "wcr",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/wcr"}
   {:uri-scheme   "webcal",
    :description  "webcal",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/webcal"}
   {:uri-scheme   "web+ap",
    :description  "web+ap",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/web+ap"}
   {:uri-scheme   "wifi",
    :description  "wifi",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/wifi"}
   {:uri-scheme   "wpid",
    :description  "wpid",
    :status       :historical,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/wpid"}
   {:uri-scheme             "ws",
    :description            "WebSocket connections",
    :status                 :permanent,
    :well-known-uri-support "https://www.rfc-editor.org/rfc/rfc8307.html",
    :references             ["https://www.rfc-editor.org/rfc/rfc6455.html"]}
   {:uri-scheme             "wss",
    :description            "Encrypted WebSocket connections",
    :status                 :permanent,
    :well-known-uri-support "https://www.rfc-editor.org/rfc/rfc8307.html",
    :references             ["https://www.rfc-editor.org/rfc/rfc6455.html"]}
   {:uri-scheme   "wtai",
    :description  "wtai",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/wtai"}
   {:uri-scheme   "wyciwyg",
    :description  "wyciwyg",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/wyciwyg"}
   {:uri-scheme  "xcon",
    :description "xcon",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc6501.html"]}
   {:uri-scheme  "xcon-userid",
    :description "xcon-userid",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc6501.html"]}
   {:uri-scheme   "xfire",
    :description  "xfire",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/xfire"}
   {:uri-scheme  "xmlrpc.beep",
    :description "xmlrpc.beep",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3529.html"]}
   {:uri-scheme  "xmlrpc.beeps",
    :description "xmlrpc.beeps",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc3529.html"]}
   {:uri-scheme  "xmpp",
    :description "Extensible Messaging and Presence Protocol",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc5122.html"]}
   {:uri-scheme   "xftp",
    :description  "xftp",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/xftp"}
   {:uri-scheme   "xrcp",
    :description  "xrcp",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/xrcp"}
   {:uri-scheme   "xri",
    :description  "xri",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/xri"}
   {:uri-scheme   "ymsgr",
    :description  "ymsgr",
    :status       :provisional,
    :template-url "https://www.iana.org/assignments/uri-schemes/prov/ymsgr"}
   {:uri-scheme  "z39.50",
    :description "Z39.50 information access",
    :status      :historical}
   {:uri-scheme  "z39.50r",
    :description "Z39.50 Retrieval",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc2056.html"]}
   {:uri-scheme  "z39.50s",
    :description "Z39.50 Session",
    :status      :permanent,
    :references  ["https://www.rfc-editor.org/rfc/rfc2056.html"]}])

(defn uri-scheme-details
  ([] (uri-scheme-details {}))
  ([{:keys [status obsolete?]}]
   (let [filter-fn (constantly true)
         filter-fn
         (if status
           (fn [x] (and (filter-fn x) (= (:status x) status)))
           filter-fn)
         filter-fn
         (cond
           (true? obsolete?)
           (fn [x] (and (filter-fn x) (= (:obsolete? x) true)))

           (false? obsolete?)
           (fn [x] (and (filter-fn x) (nil? (:obsolete? x))))

           :else
           filter-fn)]
     (filter filter-fn uri-scheme-data))))

(defn uri-schemes
  ([] (uri-schemes {}))
  ([options]
   (mapv :uri-scheme (uri-scheme-details options))))
