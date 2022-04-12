(define-module (inria sundials_test)
      #:use-module (guix)
      #:use-module (guix git-download)
      #:use-module (guix build-system cmake)
      #:use-module (gnu packages gcc)
      #:use-module (gnu packages maths)
      #:use-module (gnu packages python)
      #:use-module (gnu packages mpi))
      
(define-public sundials_test
  (package
    (name "sundials_test")
    (version "6.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://computation.llnl.gov/projects/sundials/download/"
                           "sundials-" version ".tar.gz"))
       (sha256
        (base32
         "074hs0kiwsnvcci6p8lnm7pk8pz6xhxffyf7654yah062i99z97f"
         ))))
    (build-system cmake-build-system)
    (native-inputs
     `(("python" ,python-2)))    ;for tests; syntax incompatible with python 3
    (inputs
     `(("fortran" ,gfortran)            ;for fcmix
       ("blas" ,openblas)
       ("suitesparse" ,suitesparse)))   ;TODO: Add hypre
    (arguments
     `(#:configure-flags `("-DCMAKE_C_FLAGS=-O2 -g -fcommon"
                           "-DEXAMPLES_ENABLE_C:BOOL=ON"
                           "-DEXAMPLES_ENABLE_CXX:BOOL=ON"
                           "-DEXAMPLES_ENABLE_F77:BOOL=ON"
                           "-DEXAMPLES_ENABLE_F90:BOOL=ON"
                           "-DEXAMPLES_INSTALL:BOOL=OFF"

                           "-DFCMIX_ENABLE:BOOL=ON"

                           "-DKLU_ENABLE:BOOL=ON"
                           ,(string-append "-DKLU_INCLUDE_DIR="
                                           (assoc-ref %build-inputs "suitesparse")
                                           "/include")
                           ,(string-append "-DKLU_LIBRARY_DIR="
                                           (assoc-ref %build-inputs "suitesparse")
                                           "/lib"))))
    (home-page "https://computation.llnl.gov/projects/sundials")
    (synopsis "Suite of nonlinear and differential/algebraic equation solvers")
    (description "SUNDIALS is a family of software packages implemented with
the goal of providing robust time integrators and nonlinear solvers that can
easily be incorporated into existing simulation codes.")
    (properties
     '((release-monitoring-url
        . "https://computing.llnl.gov/projects/sundials/sundials-software")))
    (license #f)))
