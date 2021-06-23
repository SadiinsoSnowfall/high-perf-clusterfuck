(define-module (utils python-science)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (guix build-system python)
  #:use-module (guix licenses))

(define-public python-pyamg
  (package
    (name "python-pyamg")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyamg" version))
              (sha256 (base32 "12m32pqymb9w94kqgijrfj2fvj4r2nbg51llfm77fabfv3zkisrw"))))
    (build-system python-build-system)
    (arguments `(#:phases (modify-phases %standard-phases
                            (add-before 'build 'set-home
                              (lambda _
                                ;; The build process expects HOME to be set
                                ;; to a writable directory.
                                (setenv "HOME" (getcwd))
                                #t)))))

    ;; Things only needed for tests.
    (native-inputs `(("python-pytest" ,python-pytest)))
    (inputs `(("pybind11", pybind11)
              ("python-numpy", python-numpy)
              ("python-scipy", python-scipy)))
    (synopsis "PyAMG library for python.")
    (description "PyAMG is a library of Algebraic Multigrid (AMG) solvers with a convenient Python interface.")
    (home-page "https://github.com/pyamg/pyamg")
    (license gpl3+)))

(define-public python-ttpy
  (package
   (name "python-ttpy")
   (version "1.2")
   (home-page "https://github.com/oseledets/ttpy")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url home-page)
                  (commit "5fd095177f0474f8b977f15574139ca9e3acb06f")
                  (recursive? #t)))
            (sha256
             (base32
              "10apkcnc6sfa8q0snhzb2ag1qxdcsx0hp76nn1ivyhwwaj6skd8h"))))

   (build-system python-build-system)
   (arguments
    `(#:phases (modify-phases %standard-phases
                              (add-before 'build 'patch-before-build
                                          (lambda _
                                            ;; Indicate we want to use "CPU = i8-gnu"
                                            (rename-file "tt/tt-fort/Makefile.cpu.default" "tt/tt-fort/Makefile.cpu")
                                            ;; Python seems to be checking LD_LIBRARY_PATH for dependencies
                                            ;; so we copy the paths in LIBRARY_PATH to help it
                                            (setenv "LD_LIBRARY_PATH"
                                                    (getenv "LIBRARY_PATH"))
                                            #t)))

               #:tests? #f))

   (native-inputs `(("python-pytest" ,python-pytest)
                    ("python-cython",python-cython)
                    ("gfortran" ,gfortran)))
   (inputs `(("lapack", lapack)
             ("gmp", gmp)
             ("mpfr", mpfr)
             ("openblas" ,openblas)))
   (propagated-inputs `(("python-numpy" ,python-numpy)
                        ("python-scipy" ,python-scipy)
                        ("python-six" ,python-six)))

   (synopsis
    "TTPY: Python implementation of the Tensor Train (TT) - Toolbox.")
   (description
    "Python implementation of the Tensor Train (TT) -Toolbox. It contains several important packages for working with the TT-format in Python. It is able to do TT-interpolation, solve linear systems, eigenproblems, solve dynamical problems. Several computational routines are done in Fortran (which can be used separately), and are wrapped with the f2py tool.")
   (license #f))
  )
