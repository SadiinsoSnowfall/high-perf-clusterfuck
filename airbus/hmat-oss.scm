;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019 Inria

(define-module (airbus hmat-oss)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages jemalloc)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public hmat-oss
  (package
   (name "hmat-oss")
   (version "bf70719e963a858de52dade5defb49ad02f94248")
   (home-page "https://github.com/jeromerobert/hmat-oss")
   (synopsis "A hierarchical matrix C/C++ library including a LU solver.")
   (description synopsis)
   (license gpl2)
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit version)))
     (sha256
      (base32
       "0353yf6jwmgynp2c31aq7qv8j95x26apn9dmzqcdllw60l38qh2h"))))
   (build-system cmake-build-system)
   (arguments
    '(#:configure-flags '("-DBUILD_EXAMPLES=ON")))
   (inputs
    `(("jemalloc" ,jemalloc)
      ("lapack" ,openblas)))))
