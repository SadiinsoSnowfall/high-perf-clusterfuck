;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017 Inria

(define-module (inria hiepacs)
  #:use-module (inria storm)
  #:use-module (guix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public chameleon
  (package
    (name "chameleon")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://morse.gforge.inria.fr/chameleon/"
                    version "/chameleon-" version ".tar.gz"))
              (sha256
               (base32
                "0875bngwzxgycm44p7nndzsy9xcbc58rn08vq08041kzpnwhzk3x"))
              (patches (search-patches "inria/patches/chameleon-lapacke.patch"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (let ((blas (assoc-ref %build-inputs "blas"))
             (lapack (assoc-ref %build-inputs "lapack")))
         (list "-DMORSE_VERBOSE_FIND_PACKAGE=ON"
               (string-append "-DBLAS_DIR=" blas)
               (string-append "-DLAPACK_DIR=" blas)))

       ;; Sometimes morse/precision_generator/Conversion.py would be called
       ;; too early, leading to:
       ;;
       ;;    from subs import subs;
       ;; EOFError: EOF read where object expected
       #:parallel-build? #f

       #:phases  (modify-phases %standard-phases
                   (add-before 'check 'set-home
                     (lambda _
                       ;; Some of the tests use StarPU, which expects $HOME
                       ;; to be writable.
                       (setenv "HOME" (getcwd))
                       #t)))))
    (inputs `(("starpu" ,starpu)
              ("blas" ,openblas)
              ("lapack" ,lapack)
              ("mpi" ,openmpi)
              ("hwloc" ,hwloc)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("gfortran" ,gfortran)
                     ("python" ,python-2)))
    (home-page "https://project.inria.fr/chameleon/")
    (synopsis "Dense linear algebra solver")
    (description
     "Chameleon is a dense linear algebra solver relying on sequential
task-based algorithms where sub-tasks of the overall algorithms are submitted
to a run-time system.  Such a system is a layer between the application and
the hardware which handles the scheduling and the effective execution of
tasks on the processing units.  A run-time system such as StarPU is able to
manage automatically data transfers between not shared memory
area (CPUs-GPUs, distributed nodes).")
    (license license:cecill-c)))
