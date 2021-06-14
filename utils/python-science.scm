(define-module (utils python-science)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages check)
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
   (version "1.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "ttpy" version))
     (sha256
      (base32
       "1mgdq0g57f7sivl6g36p7qx99hm6zyc5hl8symdrmah1pgdywapq"))))
   (build-system python-build-system)
   (home-page "https://github.com/oseledets/ttpy")
   (synopsis
    "TTPY: Python implementation of the Tensor Train (TT) - Toolbox.")
   (description
    "Python implementation of the Tensor Train (TT) -Toolbox. It contains several important packages for working with the TT-format in Python. It is able to do TT-interpolation, solve linear systems, eigenproblems, solve dynamical problems. Several computational routines are done in Fortran (which can be used separatedly), and are wrapped with the f2py tool.")
   (license #f))
)
