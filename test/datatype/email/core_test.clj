(ns datatype.email.core-test
  (:require
   [clojure.test :refer [deftest]]

   [datatype.email.core :as dt-email]

   [spec.test-support.cases :as sv-cases]))

; email address test cases here are taken from:
; https://en.wikipedia.org/wiki/Email_address#Valid_email_addresses (2024-04-16)
(deftest email-address?
  (sv-cases/assert-cases-satisfied-by dt-email/email-address?
    (sv-cases/true-case "any valid email address"
      :samples [; simple lowercase
                "simple@example.com"

                ; simple uppercase
                "SIMPLE@EXAMPLE.COM"

                ; simple dotted lowercase
                "very.common@example.com"

                ; simple dotted uppercase
                "VERY.COMMON@EXAMPLE.COM"

                ; one-letter local-part
                "x@example.com"

                "long.email-address-with-hyphens@and.subdomains.example.com"

                ; may be routed to user.name@example.com inbox depending on
                ; mail server
                "user.name+tag+sorting@example.com"

                ; slashes are a printable character, and allowed
                "name/surname@example.com"

                ; local domain name with no TLD, although ICANN highly
                ; discourages dotless email addresses
                "admin@example"

                ; any TLDs allowed
                "example@s.example"

                ; bangified host route used for uucp mailers
                "mailhost!username@example.org"

                ; special characters
                "/#!$%&'*+-/=?^_`{}|~@example.org"

                ; % escaped mail route to user@example.com via example.org
                "user%example.com@example.org"

                ; local-part ending with non-alphanumeric character from the
                ; list of allowed printable characters
                "user-@example.org"])
    (sv-cases/true-case "any strictly invalid but allowed email address"
      :samples [; local-part is longer than 64 characters
                (str "1234567890123456789012345678901234567890123456789"
                  "012345678901234+x@example.com")])
    (sv-cases/false-case "any invalid email address"
      :samples [; no @ character
                "abc.example.com"

                ; only one @ is allowed outside quotation marks
                "a@b@c@example.com"

                ; none of the special characters in this local-part are
                ; allowed outside quotation marks
                "a\"b(c)d,e:f;g<h>i[j\\k]l@example.com"

                ; quoted strings must be dot separated or be the only element
                ; making up the local-part
                "just\"not\"right@example.com"

                ; spaces, quotes, and backslashes may only exist when within
                ; quoted strings and preceded by a backslash
                "this is\"not\\allowed@example.com"

                ; even if escaped (preceded by a backslash), spaces, quotes,
                ; and backslashes must still be contained by quotes
                "this\\ still\"not\\allowed@example.com"

                "i.like.underscores@but_they_are_not_allowed_in_this_part"])
    (sv-cases/false-case "any strictly valid but disallowed email address"
      :samples [; space between quotes
                "\" \"@example.org"

                ; quoted double dot
                "\"john..doe\"@example.org"

                ; quoted punctuation characters including @
                (str "\"very.(),:;<>[]\".VERY.\"very@\\ \"very\".unusual\"@"
                  "strange.example.com")

                ; IP addresses are allowed instead of domains when in square
                ; brackets, but strongly discouraged
                "postmaster@[123.123.123.123]"

                ; IPv6 uses a different syntax
                "postmaster@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]"

                ; begin with underscore different syntax
                "_test@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]"])
    (sv-cases/false-case "strings that aren't email address-like at all"
      :samples ["the quick brown fox jumped over the lazy dog"
                "23.6"
                "true"])
    (sv-cases/false-case "a non-string"
      :samples [true false 35.4 #{"GBP" "USD"}])
    (sv-cases/false-case "nil" :sample nil)))
