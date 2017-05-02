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
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config))

(define-public simgrid
  (package
    (name "simgrid")
    (version "3.15")
    (source (origin
              (method url-fetch)
              (uri
               "http://gforge.inria.fr/frs/download.php/file/36621/SimGrid-3.15.tar.gz")
              (sha256
               (base32
                "1s595wc4z8hkvim3ypfdxy16pply6ckjg10v84cc0lx9pz6i3r6i"))))
    (build-system cmake-build-system)
    (arguments
     '(;; Have the RUNPATH of executables point to $libdir, where
       ;; libsimgrid.so lives.
       #:configure-flags (list (string-append "-DCMAKE_EXE_LINKER_FLAGS="
                                              "-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))

       ;; FIXME: 8% of the tests fail, but they typically rely on diffing logs,
       ;; and said logs include timestamps, pointer values, file names, etc.
       #:tests? #f))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-wrapper)
       ("doxygen" ,doxygen)))
    (inputs
     `(("boost" ,boost)))
    ;; XXX: 'smpicc' and 'smpicxx' retain references to GCC.
    (home-page "http://simgrid.gforge.inria.fr/")
    (synopsis "Distributed system simulator")
    (description
     "SimGrid is a scientific instrument to study the behavior of large-scale
distributed systems such as grids, \"clouds\", HPC, and P2P systems.  It can
be used to evaluate heuristics, prototype applications or even assess legacy
MPI applications.")

    ;; 'COPYING' lists the licenses of pieces of software included in
    ;; SimGrid; they are all under non-copyleft licenses though.
    (license license:lgpl2.1+)))
