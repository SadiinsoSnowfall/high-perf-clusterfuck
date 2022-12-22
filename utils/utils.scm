;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019, 2020, 2022 Inria

(define-module (utils utils)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages xml)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  ;; To remove when/if python2 packages sympy and mpi4py
  ;; are fixed in official repo
  #:use-module (guix build-system python)
  )

(define-public r-rlist
(package
 (name "r-rlist")
 (version "0.4.6.1")
 (source
  (origin
   (method url-fetch)
   (uri (cran-uri "rlist" version))
   (sha256
    (base32
     "08awy2p7rykc272wvvya4ddszbr7b7s7qv4wr3hs8ylr4jqlh0dv"))))
 (properties `((upstream-name . "rlist")))
 (build-system r-build-system)
 (propagated-inputs
  (list r-data-table r-jsonlite r-xml r-yaml))
 (home-page "https://renkun.me/rlist")
 (synopsis
  "A Toolbox for Non-Tabular Data Manipulation")
 (description
  "This package provides a set of functions for data manipulation with list objects, including mapping, filtering, grouping, sorting, updating, searching, and other useful functions.  Most functions are designed to be pipeline friendly so that data processing with lists can be chained.")
 (license license:expat)))

(define-public sz-compressor
  (package
   (name "sz-compressor")
   (version "2.1.11")
   (home-page "https://github.com/szcompressor/SZ")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url home-page)
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256 (base32 "0kzmiigh12aysaq5nnapsh5vrcpvqc7nl21km4hz4xrmp94n2x9c"))))
   (build-system cmake-build-system)
   (arguments
    '(#:build-type "Release"))
   (synopsis "GUIX package for the SZ compressor.")
   (description "GUIX package for the SZ compressor.")
   (license license:gpl3+)))

(define-public python-expecttest
  (package
   (name "python-expecttest")
   (version "0.1.3")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "expecttest" version))
     (sha256
      (base32
       "16hlaymwnwz0iqghfh7aj850xf8d0x50kv8kxn51550xh6apc1c3"))))
   (build-system python-build-system)
   (home-page "https://github.com/ezyang/ghstack")
   (synopsis "This library implements expect tests (also known as 'golden'
tests).")
   (description "Expect tests are a method of writing tests where instead of
hard-coding the expected output of a test, you run the test to get the output,
and the test framework automatically populates the expected output.")
   (license license:expat)))

(define-public hiredis
  (package
    (name "hiredis")
    (version "1.0.2")
    (home-page "https://github.com/redis/hiredis")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0a55zk3qrw9yl27i87h3brg2hskmmzbfda77dhq9a4if7y70xnfb"))))
    (build-system cmake-build-system)
    (arguments
     '(#:build-type "Release"))
    (native-inputs (list redis))
    (synopsis "Minimalistic C client for Redis >= 1.2")
    (description
     "Hiredis is a minimalistic C client library for the Redis database.
It is minimalistic because it just adds minimal support for the protocol, but
at the same time it uses a high level printf-alike API in order to make it much
higher level than otherwise suggested by its minimal code base and the lack of
explicit bindings for every Redis command.
Apart from supporting sending commands and receiving replies, it comes with a
reply parser that is decoupled from the I/O layer. It is a stream parser
designed for easy reusability, which can for instance be used in higher level
language bindings for efficient reply parsing.")
    (license license:bsd-3)))

(define-public redox
  (package
    (name "redox")
    (version "0.3") ;no official releases, according to HISTORY.md
    (home-page "https://github.com/mpoquet/redox") ;this fork contains additionnal commits to generate pkg-config files
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "e7904da79d5360ba22fbab64b96be167b6dda5f6")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mmxrjfidcm5fq233wsgjb9rj81hq78rn52c02vwfmz8ax9bc5yg"))))
    (build-system cmake-build-system)
    (arguments
     '(#:build-type "Release"
       #:tests? #f))
    (propagated-inputs (list libev hiredis))
    (synopsis "Modern, asynchronous, and wicked fast C++11 client for Redis")
    (description
     "Redox is a C++ interface to the Redis key-value store that makes it easy
     to write applications that are both elegant and high-performance.
     Communication should be a means to an end, not something we spend a lot of
     time worrying about. Redox takes care of the details so you can move on to
     the interesting part of your project.")
    (license license:asl2.0)))

(define-public cpp-docopt
  (package
    (name "cpp-docopt")
    (version "0.6.3")
    (home-page "https://github.com/docopt/docopt.cpp")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cz3vv7g5snfbsqcf3q8bmd6kv5qp84gj3avwkn4vl00krw13bl7"))))
    (build-system cmake-build-system)
    (arguments
     '(#:build-type "Release"
       #:tests? #f))
    (synopsis "C++11 port of docopt")
    (description
      "docopt helps you:
- define the interface for your command-line app, and
- automatically generate a parser for it.
docopt is based on conventions that have been used for decades in help messages
and man pages for describing a program's interface. An interface description in
docopt is such a help message, but formalized.")
    (license (list license:expat license:boost1.0))))
