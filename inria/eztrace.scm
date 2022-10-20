;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019, 2020 Inria

(define-module (inria eztrace)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages) ; for guix style
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mpi))

(define-public eztrace
  (package
    (name "eztrace")
    (version "1.1-10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/eztrace/eztrace")
                    (commit (string-append "eztrace-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m0gw183nka2z7mkski1zzphdyqggxgi0blrgml6dww3z3bqhlic"))
              ;; (modules '((guix build utils)))

              ;; Remove bundled libraries.
              ;; FIXME: There's few more under extlib/.
              ;; FIXME: The bundled litl is different from the latest
              ;; release (0.1.8), so we have to use it.
              ;; (snippet '(delete-file-recursively "extlib/litl"))
              ))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list "LDFLAGS=-liberty" ;for bfd
                               ;; (string-append "--with-litl="
                               ;;                (assoc-ref %build-inputs
                               ;;                           "litl"))
                               (string-append "--with-mpi="
                                              (assoc-ref %build-inputs
                                                         "openmpi")))

       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'make-source-writable
                    (lambda _
                      ;; Make sure 'autoreconf' can write 'configure' files.
                      (for-each make-file-writable
                                (find-files "." "^configure$"))
                      #t))
                  (add-before 'configure 'ensure-ld-wrapper-is-first
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; Make sure the ld wrapper comes before the 'ld'
                      ;; command of BINUTILS-2.33.
                      (let ((ld-wrapper (assoc-ref inputs "ld-wrapper")))
                        (setenv "PATH"
                                (string-append ld-wrapper "/bin:"
                                               (getenv "PATH")))
                        #t)))
                  (add-before 'bootstrap 'patch-build-tool-shebangs
                    (lambda _
                      ;; These scripts are executed from 'autoreconf'.
                      (for-each patch-shebang
                                (find-files "." "\\.sh$"))
                      #t)))

       ;; FIXME: There are test failures in bundled libraries.
       #:tests? #f))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (inputs `(("litl" ,litl)
              ("gfortran" ,gfortran)
              ("libiberty" ,libiberty)            ;for bfd
              ("zlib" ,zlib)                      ;for bfd

              ;; Pptrace needs 'bfd_get_section', which is no longer
              ;; available in Binutils 2.34.
              ("binutils-2.33" ,binutils-2.33)

              ("openmpi" ,openmpi)))
    (synopsis "Collect program execution traces")
    (description
     "EZTrace is a tool that aims at generating automatically execution trace
from high performance computing (HPC) programs.  It generates execution trace
files that can be interpreted by visualization tools such as
@uref{https://solverstack.gitlabpages.inria.fr/vite/, ViTE}.")
    (license license:cecill-c)                    ;FIXME: really CECILL-B
    (home-page "https://eztrace.gitlab.io/eztrace/")))

(define-public litl
  (package
    (name "litl")
    (home-page "https://github.com/trahay/LiTL")
    (version "0.1.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    ;; this commit is a little bit after litl-0.1.9, but it
                    ;; fixes some issues
                    (commit "05ed9f59f00a9af0a08894d7e972465239d26e6d")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qwymk8f8qhaq7z1vxwafwph828lfxdvsmsp7c3inpxxy6ig6qn9"))))
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
    (native-inputs (list autoconf automake libtool))
    (license license:bsd-2)))

(define-public fxt
  (package
    (name "fxt")
    (version "0.3.14")
    (source (origin
              (uri (string-append "mirror://savannah/fkt/fxt-"
                                  version ".tar.gz"))
              (method url-fetch)
              (sha256
               (base32
                "1wpbmax4jzc454ilz9vay0a63ilxsav910rvqizg5naw2y9qsz9i"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl) ("help2man" ,help2man)))
    (home-page "https://savannah.nongnu.org/projects/fkt")
    (synopsis "Efficient recording of program execution traces")
    (description
     "FxT is a fast tracing engine that can be used either in user land, in
kernel land, or both.  It can record developer-specified events in compact
\"traces\", with minimal run-time overhead.")
    (license license:gpl2+)))

(define-public fxt+static
  (package
    (inherit fxt)
    (name "fxt-static")
    (arguments
     '(#:configure-flags '("CFLAGS=-fPIC"
                           "--enable-static=yes"
                           "--enable-shared=no")))))
