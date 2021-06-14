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
   (version "1.0")
   (home-page "https://github.com/oseledets/ttpy")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url home-page)
                  (commit "b440f6299a6338de4aea67f3d839d613f4ef1374")
                  (recursive? #t)))
            (sha256
             (base32
              "1bbq3acyxdjf33qj75yxrcz4aqzr8kzzzd70vzdcg3x5z943xi1k"))))
   ;; (version "1.2.1")
   ;; (source
   ;;  (origin
   ;;   (method url-fetch)
   ;;   (uri (pypi-uri "ttpy" version))
   ;;   (sha256
   ;;    (base32
   ;;     "1mgdq0g57f7sivl6g36p7qx99hm6zyc5hl8symdrmah1pgdywapq"))))
   (build-system python-build-system)
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
;;    (arguments
;;     `(#:phases
;;       (modify-phases %standard-phases
;;                      (add-before 'build 'configure-openblas
;;                                  (lambda* (#:key inputs #:allow-other-keys)
;;                                           (call-with-output-file "site.cfg"
;;                                             (lambda (port)
;;                                               (format port
;;                                                       "[blas]
;; libraries = openblas
;; library_dirs = ~a/lib
;; include_dirs = ~a/include

;; # backslash-n to make emacs happy
;; \n[atlas]
;; library_dirs = ~a/lib
;; atlas_libs = openblas
;; "
;;                                                       (assoc-ref inputs "openblas")
;;                                                       (assoc-ref inputs "openblas")
;;                                                       (assoc-ref inputs "openblas"))))
;;                                           #t)))))
   (synopsis
    "TTPY: Python implementation of the Tensor Train (TT) - Toolbox.")
   (description
    "Python implementation of the Tensor Train (TT) -Toolbox. It contains several important packages for working with the TT-format in Python. It is able to do TT-interpolation, solve linear systems, eigenproblems, solve dynamical problems. Several computational routines are done in Fortran (which can be used separately), and are wrapped with the f2py tool.")
   (license #f))
)
