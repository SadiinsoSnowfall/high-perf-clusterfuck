;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019 Inria

(define-module (inria hiepacs)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (inria storm)
  #:use-module (inria storm-pm2)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public chameleon
  (package
    (name "chameleon")
    (version "0.9.2")
    (home-page "https://gitlab.inria.fr/solverstack/chameleon")
    (synopsis "Dense linear algebra solver")
    (description
     "Chameleon is a dense linear algebra solver relying on sequential
task-based algorithms where sub-tasks of the overall algorithms are submitted
to a run-time system.  Such a system is a layer between the application and
the hardware which handles the scheduling and the effective execution of
tasks on the processing units.  A run-time system such as StarPU is able to
manage automatically data transfers between not shared memory
area (CPUs-GPUs, distributed nodes).")
    (license license:cecill-c)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "d2b4cab3a6b860f068979ecc2a532a3249060ba8")
                    ;; We need the submodule in 'CMakeModules/morse_cmake'.
                    (recursive? #t)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "03sjykh24ms4h2vzylkxcc6v7nshl3w0dhyyrv9grzckmxvmvzij"))))
    (build-system cmake-build-system)
    (outputs '("debug" "out"))
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DCHAMELEON_USE_MPI=ON")

       ;; FIXME: Test too long for gitlab-runner CI
       #:tests? #f

       #:phases  (modify-phases %standard-phases
                   (add-before 'check 'set-home
                     (lambda _
                       ;; Some of the tests use StarPU, which expects $HOME
                       ;; to be writable.
                       (setenv "HOME" (getcwd))
                       #t)))))
    (inputs `(("lapack" ,openblas)))
    (propagated-inputs `(("starpu" ,starpu)
                         ("mpi" ,openmpi)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("gfortran" ,gfortran)
                     ("python" ,python-2)))))

(define-public maphys
  (package
    (name "maphys")
    (version "0.9.6.0")
    (home-page "https://gitlab.inria.fr/solverstack/maphys/maphys")
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
                "020r2km34n3pxrg3iw4djdjbzq1y61q7gnaz92mi1bbmyk0hs5ip"))
              (patches (search-patches "inria/patches/maphys-installation-directories.patch"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DMAPHYS_SDS_MUMPS=ON"
                           "-DMAPHYS_SDS_PASTIX=OFF")

       ;; FIXME: Tests segfault.
       #:tests? #f))
    (inputs `(("hwloc" ,hwloc "lib")
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

(define-public pastix
  (let ((commit "2f30ff07a45fefa35b117783c3d9d0913cc75552"))
    (package
      (name "pastix")
      (version "6.0.0")
      (home-page "https://gitlab.inria.fr/solverstack/pastix")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)

                      ;; We need the submodule in 'cmake_modules/morse'.
                      (recursive? #t)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "106rf402cvfdhc2yfywhjqbbx8ghn3rmxkrjqgmsbrnarbxhiki9"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                             "-DPASTIX_WITH_STARPU=ON")

         #:phases (modify-phases %standard-phases
                    (add-before 'check 'prepare-test-environment
                      (lambda _
                        ;; StarPU expects $HOME to be writable.
                        (setenv "HOME" (getcwd))

                        ;; The Python-driven tests want to dlopen PaSTiX
                        ;; libraries (via ctypes) so we need to help them.
                        (let* ((libraries   (find-files "." "\\.so$"))
                               (directories (map (compose canonicalize-path
                                                          dirname)
                                                 libraries)))
                          (setenv "LD_LIBRARY_PATH"
                                  (string-join directories ":"))

                          ;; 'ctypes.util.find_library' tries to link with
                          ;; -lpastix.
                          (setenv "LIBRARY_PATH"
                                  (string-append (getenv "LIBRARY_PATH") ":"
                                                 (getenv "LD_LIBRARY_PATH")))
                          #t))))))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("gfortran" ,gfortran)))
      (inputs
       `(("gfortran:lib" ,gfortran "lib")         ;for 'gcc … -lgfortran'
         ("hwloc" ,hwloc "lib")
         ("openblas" ,openblas)
         ("lapack" ,lapack)       ;must be built with '-DLAPACKE_WITH_TMG=ON'
         ("scotch" ,scotch)

         ;; The following are optional dependencies.
         ("metis" ,metis)
         ("starpu" ,starpu)

         ;; Python bindings and Python tests.
         ("python" ,python-wrapper)
         ("python-numpy" ,python-numpy)
         ("python-scipy" ,python-scipy)))
      (synopsis "Sparse matrix direct solver")
      (description
       "PaStiX (Parallel Sparse matriX package) is a scientific library that
provides a high performance parallel solver for very large sparse linear
systems based on direct methods.  Numerical algorithms are implemented in
single or double precision (real or complex) using LLt, LDLt and LU with
static pivoting (for non symmetric matrices having a symmetric pattern).
This solver also provides some low-rank compression methods to reduce the
memory footprint and/or the time-to-solution.")
      (license license:cecill))))
