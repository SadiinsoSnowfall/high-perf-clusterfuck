;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019, 2021 Inria

(define-module (airbus hmat-oss)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages jemalloc)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public hmat-oss
  (let
      ((commit "6a4e21bb86cda66396f27d3473471c52a2357edc")
       (revision "32"))
    (package
     (name "hmat-oss")
     (version
      (git-version "1.7.1" revision commit))
     (home-page "https://github.com/jeromerobert/hmat-oss")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (sha256
        (base32
         "1alkzahpw55bqy01yscvl7sl2r4mhyv7i1qs12wmzhapqsw36jvg"))))
     (build-system cmake-build-system)
     (arguments
      '(#:configure-flags '("-DBUILD_EXAMPLES=ON")))
     (inputs
      `(("jemalloc" ,jemalloc)
        ("lapack" ,openblas)))
     (synopsis "A hierarchical matrix C/C++ library including an LU solver.")
     (description #f)
     (license license:gpl2))))
