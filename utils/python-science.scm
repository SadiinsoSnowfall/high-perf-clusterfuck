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
  #:use-module (gnu packages machine-learning)
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
                                            ;; Removing lapack dependency
                                            (substitute* "tt/__init__.py"
                                                         (("liblapack.so")  "libopenblas.so"))
                                            ;; Python seems to be checking LD_LIBRARY_PATH for dependencies
                                            ;; so we copy the paths in LIBRARY_PATH to help it
                                            (setenv "LD_LIBRARY_PATH"
                                                    (getenv "LIBRARY_PATH"))
                                            #t)))

               #:tests? #f))

   (native-inputs `(("python-pytest" ,python-pytest)
                    ("python-cython",python-cython)
                    ("gfortran" ,gfortran)))
   (inputs `(("gmp", gmp)
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

(define-public python-easydict
  (package
    (name "python-easydict")
    (version "1.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "easydict" version))
        (sha256
          (base32
            "0fw82sjzki95rl5f6nscz9bvp1fri3rv2b83vzsg16f20ymhsgrz"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/makinacorpus/easydict")
    (synopsis
      "Access dict values as attributes (works recursively).")
    (description
      "Access dict values as attributes (works recursively).")
    (license #f)))

(define-public python-pillow6
  (package
   (inherit python-pillow)
   (name "python-pillow")
   (version "6.1.0")
   (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Pillow" version))
       (sha256
        (base32
         "1pnrsz0f0n0c819v1pdr8j6rm8xvhc9f3kh1fv9xpdp9n5ygf108"))))
   (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-ldconfig
           (lambda _
             (substitute* "setup.py"
               (("\\['/sbin/ldconfig', '-p'\\]") "['true']"))))
         (replace 'check
           (lambda* (#:key outputs inputs tests? #:allow-other-keys)
             (when tests?
               (setenv "HOME" (getcwd))
               ;; Make installed package available for running the tests.
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "selftest.py" "--installed")))))))))

(define-public python-torch-vision
  (package
    (name "python-torch-vision")
    (version "0.2.2")
    (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/pytorch/vision")
          (commit (string-append "v" version))))
        (sha256
          (base32 "0wmpvb67a3778syxk7wav1rajf0ad71f85vmwbvrxw2nc2agxsd9"))))
    (build-system python-build-system)
    (arguments
     ;; The 'check' phase is producing a 'not a test' error! 
     '(#:tests? #f))
    (inputs
     `(("python-pytorch" ,python-pytorch)
       ("python-pillow" ,python-pillow6)
       ("python-scipy" ,python-scipy)))
    (home-page "https://github.com/pytorch/vision")
    (synopsis "image and video datasets and models for torch deep learning")
    (description "image and video datasets and models for torch deep learning")
    (license bsd-3)))

(define-public python-torch-diffeq
  (package
    (name "python-torch-diffeq")
    (version "0.2.2")
    (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/rtqichen/torchdiffeq")
          (commit "97e93deddcb18f67330f0b9caa75808f38b94c89")))
        (sha256
          (base32 "04gmc13jf0wnbdvslgvzzbnnmzl1f7q44b73xbpaa7s7s4ijprxd"))))
    (build-system python-build-system)
    (arguments
     ;; Looks like the tests require network connection.
     '(#:tests? #f))
    (inputs
     `(("python-pytorch" ,python-pytorch)
       ("python-pillow" ,python-pillow6)
       ("python-scipy" ,python-scipy)))
    (home-page
      "https://github.com/rtqichen/torchdiffeq")
    (synopsis
      "Differentiable ODE solvers with full GPU support and O(1)-memory
backpropagation.")
    (description
      "This library provides ordinary differential equation (ODE) solvers
implemented in PyTorch. Backpropagation through ODE solutions is supported using
the adjoint method for constant memory cost. For usage of ODE solvers in deep
learning applications.

As the solvers are implemented in PyTorch, algorithms in this repository are
fully supported to run on the GPU.")
    (license expat)))
