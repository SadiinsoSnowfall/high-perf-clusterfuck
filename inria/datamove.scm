;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2022 Inria

(define-module (inria datamove)
  #:use-module (guix)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (inria simgrid)
  #:use-module (utils utils))

(define-public intervalset
  (package
    (name "intervalset")
    (version "1.2.0")
    (home-page "https://framagit.org/batsim/intervalset")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ayj6jjznbd0kwacz6dki6yk4rxdssapmz4gd8qh1yq1z1qbjqgs"))))
    (build-system meson-build-system)
    (native-inputs (list boost pkg-config))
    (synopsis "C++ library to manage sets of integral closed intervals")
    (description
     "intervalset is a C++ library to manage sets of closed intervals of integers.
      This is a simple wrapper around Boost.Icl.")
    (license license:lgpl3)))

(define-public batsim
  (package
    (name "batsim")
    (version "4.1.0")
    (home-page "https://framagit.org/batsim/batsim")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zhl0f22hyljxd63kirv4b071bx3smikd8ci1flky752dffyrdk4"))))
    (build-system meson-build-system)
    (native-inputs (list boost pkg-config))
    (propagated-inputs (list simgrid
                             zeromq
                             redox
                             rapidjson
                             pugixml
                             cpp-docopt
                             intervalset))
    (synopsis
     "An infrastructure simulator that focuses on resource management techniques.")
    (description
     "Batsim is an infrastructure simulator that enables the study of resource management techniques.")
    (license license:lgpl3)))
