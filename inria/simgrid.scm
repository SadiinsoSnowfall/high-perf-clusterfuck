;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017 Inria

(define-module (inria simgrid)
  #:use-module (guix)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config))

(define-public simgrid
  (package
    (name "simgrid")
    (version "3.32")
    (source (origin
              (method url-fetch)
              (uri
               "https://framagit.org/simgrid/simgrid/uploads/c45f7fd6872b3b0d26b9ba2e607d6e3a/simgrid-3.32.tar.gz")
              (sha256
               (base32
                "1w21lf5gmivybray9900vya1zsyr322vs3yjkpj08bsnh7mn8xw3"))))
    (build-system cmake-build-system)
    (arguments
     '(;; Have the RUNPATH of executables point to $libdir, where
       ;; libsimgrid.so lives.
       #:configure-flags (list "-Denable_msg=ON"
                               (string-append "-DCMAKE_EXE_LINKER_FLAGS="
                                              "-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))

       ;; FIXME: 8% of the tests fail, but they typically rely on diffing logs,
       ;; and said logs include timestamps, pointer values, file names, etc.
       #:tests? #f))
    (native-inputs
     (list gfortran
           `(,gfortran "lib") perl python-wrapper doxygen))
    (propagated-inputs
     (list boost))
    ;; XXX: 'smpicc' and 'smpicxx' retain references to GCC.
    (home-page "https://simgrid.org/")
    (synopsis "Distributed system simulator")
    (description
     "SimGrid is a scientific instrument to study the behavior of large-scale
distributed systems such as grids, \"clouds\", HPC, and P2P systems.  It can
be used to evaluate heuristics, prototype applications or even assess legacy
MPI applications.")

    ;; 'COPYING' lists the licenses of pieces of software included in
    ;; SimGrid; they are all under non-copyleft licenses though.
    (license license:lgpl2.1+)))

(define-public simgrid-29
  (package
    (inherit simgrid)
    (name "simgrid")
    (version "3.29")
    (source (origin
              (method url-fetch)
              (uri
               "https://framagit.org/simgrid/simgrid/uploads/6ca357e80bd4d401bff16367ff1d3dcc/simgrid-3.29.tar.gz")
              (sha256
               (base32
                "1krcv8smbgy60rccklzcg1kr1irnh2w3f1swviqfnpjmagbazs43"))))))
