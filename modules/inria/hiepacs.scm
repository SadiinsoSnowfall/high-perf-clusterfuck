;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017 Inria

(define-module (inria hiepacs)
  #:use-module (inria storm)
  #:use-module (guix)
  #:use-module (guix git-download)
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

(define-public maphys
  (package
    (name "maphys")
    (version "0.9.5.1")
    (home-page "https://gitlab.inria.fr/solverstack/maphys")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)

                    ;; We need the submodule in 'cmake_modules/morse'.
                    (recursive? #t)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0qrm5zm0bi0g3d8gbk4bvjr094k9m2pzq4xbki0xbfm5dr7xlzf9"))
              (patches (search-patches "inria/patches/maphys-installation-directories.patch"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DMAPHYS_SDS_MUMPS=ON"
                           "-DMAPHYS_SDS_PASTIX=OFF")

       ;; FIXME: Tests segfault.
       #:tests? #f))
    (inputs `(("hwloc" ,hwloc)
              ("openmpi" ,openmpi)
              ("scalapack" ,scalapack)
              ("openblas" ,openblas)
              ("lapack" ,lapack)
              ("scotch" ,pt-scotch)
              ("mumps" ,mumps)
              ("metis" ,metis)))
    (native-inputs `(("gforgran" ,gfortran)
                     ("pkg-config" ,pkg-config)))
    (synopsis "Massively Parallel Hybrid Solver")
    (description
     "MaPHyS (Massively Parallel Hybrid Solver) is a parallel linear solver
that couples direct and iterative approaches.  The underlying idea is to
apply to general unstructured linear systems domain decomposition ideas
developed for the solution of linear systems arising from PDEs.  The
interface problem, associated with the so called Schur complement system, is
solved using a block preconditioner with overlap between the blocks that is
referred to as Algebraic Additive Schwarz.  To cope with the possible lack of
coarse grid mechanism that enables one to keep constant the number of
iterations when the number of blocks is increased, the solver exploits two
levels of parallelism (between the blocks and within the treatment of the
blocks).  This enables it to exploit a large number of processors with a
moderate number of blocks which ensures a reasonable convergence behavior.")
    (license #f)))                        ;XXX: license needs to be clarified
