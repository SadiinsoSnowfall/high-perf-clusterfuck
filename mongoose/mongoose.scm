;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019, 2021 Inria

(define-module (mongoose mongoose)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake))

(define-public mongoose
  (package
   (name "mongoose")
   (version "2.0.4")
   (home-page "https://github.com/ScottKolo/Mongoose")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit (string-append "v" version))))
     (sha256
      (base32
       "0ymwd4n8p8s0ndh1vcbmjcsm0x2cc2b7v3baww5y6as12873bcrh"))))
   (build-system cmake-build-system)
   (arguments
    ;; Tests seem to require internet connection.
    '(#:tests? #f))
   (synopsis "Graph Coarsening and Partitioning Library")
   (description "Mongoose is a graph partitioning library. Currently, Mongoose
only supports edge partitioning, but in the future a vertex separator extension
will be added.")
   (license license:gpl3)))
