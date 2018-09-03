;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017 Inria

(define-module (inria eztrace)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config))

(define-public eztrace
  (package
    (name "eztrace")
    (version "1.1-4")
    (source (origin
              (uri "https://gforge.inria.fr/frs/download.php/file/36126/eztrace-1.1-4.tar.gz")
              (method url-fetch)
              (sha256
               (base32
                "0hnf9kgfqck4ik40wm54yyny6h2gi8pm7jlcb3a4ix54zwz492vf"))
              (modules '((guix build utils)))

              ;; Remove bundled libraries.
              ;; FIXME: There's few more under extlib/.
              (snippet '(delete-file-recursively "extlib/litl"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list "LDFLAGS=-liberty" ;for bfd
                               (string-append "--with-litl="
                                              (assoc-ref %build-inputs
                                                         "litl")))

       ;; FIXME: There are test failures in bundled libraries.
       #:tests? #f))
    (inputs `(("litl" ,litl)
              ("gfortran" ,gfortran)
              ("libiberty" ,libiberty)            ;for bfd
              ("zlib" ,zlib)))                    ;for bfd
    (synopsis "Collect program execution traces")
    (description
     "EZTrace is a tool that aims at generating automatically execution trace
from high performance computing (HPC) programs.  It generates execution trace
files that can be interpreted by visualization tools such as
@uref{http://vite.gforge.inria.fr/, ViTE}.")
    (license license:cecill-c)                    ;FIXME: really CECILL-B
    (home-page "http://eztrace.gforge.inria.fr/")))

(define-public litl
  (package
    (name "litl")
    (version "0.1.8")
    (source (origin
              (uri
               "https://fusionforge.int-evry.fr/frs/download.php/file/16/litl-0.1.8.tar.gz")
              (method url-fetch)
              (sha256
               (base32
                "0rbrqm164p3cf8q49bdwvry1i70awx5iyyksvr6acmzb9rkzw34v"))))
    (build-system gnu-build-system)
    (arguments
     ;; Tests expect to be run sequentially: 'test_litl_write' creates a file
     ;; that 'test_litl_read' reads.
     '(#:parallel-tests? #f))
    (synopsis "Collect events during the execution of a program")
    (description
     "This project aims at providing an alternative solution to the already
existing FxT library, which is used to record events during the execution of
scientific applications, that would deliver nearly the same performance and
would solve the scalability issues such as scalability and the number of
threads.")
    (home-page "https://fusionforge.int-evry.fr/projects/litl/")
    (license license:bsd-2)))

(define-public fxt
  (package
    (name "fxt")
    (version "0.3.3")
    (source (origin
              (uri (string-append "mirror://savannah/fkt/fxt-"
                                  version ".tar.gz"))
              (method url-fetch)
              (sha256
               (base32
                "09m6sq2qv995pv6qjrwi7582lllrbhv44qkf95a2l96c259flvrz"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)))
    (home-page "https://savannah.nongnu.org/projects/fkt")
    (synopsis "Efficient recording of program execution traces")
    (description
     "FxT is a fast tracing engine that can be used either in user land, in
kernel land, or both.  It can record developer-specified events in compact
\"traces\", with minimal run-time overhead.")
    (license license:gpl2+)))
