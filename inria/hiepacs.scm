;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019, 2021, 2022 Inria

(define-module (inria hiepacs)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pretty-print)
  #:use-module (inria mpi)
  #:use-module (inria storm)
  #:use-module (inria tadaam)
  #:use-module (inria eztrace)
  #:use-module (inria simgrid)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (lrz librsb)
  ;; To remove when/if python2 packages sympy and mpi4py
  ;; are fixed in official repo
  #:use-module (guix build-system python)
  #:use-module (gnu packages python-science)
  )

(define-public parsec
  (let ((commit "6022a61dc96c25f11dd2aeabff2a5b3d7bce867d")
        (revision "0"))
    (package
      (name "parsec")
      (version (git-version "0.0" revision commit))
      (home-page "https://bitbucket.org/mfaverge/parsec.git")
      (synopsis "Runtime system based on dynamic task generation mechanism")
      (description
       "PaRSEC is a generic framework for architecture aware scheduling
and management of micro-tasks on distributed many-core heterogeneous
architectures.")
      (license license:bsd-2)
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page)
                                    (commit commit)
                                    (recursive? #t)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "10w6ma0r6fdfav5za8yv6m5qhqvcvka5raiz2x38r42snwj0i4c8"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                             "-DPARSEC_GPU_WITH_CUDA=OFF"
                             "-DPARSEC_DIST_WITH_MPI=OFF")
         #:tests? #f))
      (inputs (list hwloc bison flex))
      (native-inputs (list gfortran python-2)))))

(define-public parsec+mpi
  (package
    (inherit parsec)
    (name "parsec-mpi")
    (arguments
     (substitute-keyword-arguments (package-arguments parsec)
                                   ((#:configure-flags flags '())
                                    `(cons "-DPARSEC_DIST_WITH_MPI=ON" (delete "-DPARSEC_DIST_WITH_MPI=OFF" ,flags)))))
    (propagated-inputs (modify-inputs (package-inputs parsec)
                         (prepend openmpi)))))

(define-public quark
  (let ((commit "db4aef9a66a00487d849cf8591927dcebe18ef2f")
        (revision "0"))
    (package
      (name "quark")             ;XXX: there's a same-named package in 'guix'
      (version (git-version "0.0" revision commit))
      (home-page "https://github.com/ecrc/quark")
      (synopsis "QUeuing And Runtime for Kernels")
      (description
       "QUARK (QUeuing And Runtime for Kernels) provides a library that
enables the dynamic execution of tasks with data dependencies in a
multi-core, multi-socket, shared-memory environment.  QUARK infers
data dependencies and precedence constraints between tasks from the
way that the data is used, and then executes the tasks in an
asynchronous, dynamic fashion in order to achieve a high utilization
of the available resources.")
      (license license:bsd-2)
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1bwh8247d70lmbr13h5cb8fpr6m0k9vcaim4bq7j8mynfclb6r77"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-makefile
             (lambda _
               (substitute* "CMakeLists.txt"
                 (("DESTINATION quark")  "DESTINATION include"))
               #t)))
         ;; No target for tests
         #:tests? #f))
      (propagated-inputs (list `(,hwloc "lib")))
      (native-inputs (list gfortran)))))

(define-public chameleon
  (package
    (name "chameleon")
    (version "1.1.0")
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
                    (commit "4db899ca30d29927018d83964b9b6d517269abe1")
                    ;; We need the submodule in 'CMakeModules/morse_cmake'.
                    (recursive? #t)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0mpnacmkn1287c003a6n3c4r0n395l6fnjilzi7z53lb34s8kaap"))))
    (build-system cmake-build-system)
    (outputs '("debug" "out"))
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DCHAMELEON_USE_MPI=ON")

       ;; FIXME: MPI tests too long for gitlab-runner CI
       #:tests? #f

       #:phases  (modify-phases %standard-phases
                                ;; Without this variable, pkg-config removes paths in already in CFLAGS
                                ;; However, gfortran does not check CPATH to find fortran modules
                                ;; and and the module fabulous_mod cannot be found
                                (add-before 'configure 'fix-pkg-config-env
                                            (lambda _ (setenv "PKG_CONFIG_ALLOW_SYSTEM_CFLAGS" "1") #t))
                                ;; Allow tests with more MPI processes than available CPU cores,
                                ;; which is not allowed by default by OpenMPI
                                (add-before 'check 'prepare-test-environment
                                            (lambda _
                                              (setenv "OMPI_MCA_rmaps_base_oversubscribe" "1") #t))
                                ;; Some of the tests use StarPU, which expects $HOME
                                ;; to be writable.
                                (add-before 'check 'set-home
                                            (lambda _
                                              (setenv "HOME" (getcwd))
                                              #t)))))
    (inputs (list openblas))
    (propagated-inputs (list starpu openmpi))
    (native-inputs (list pkg-config gfortran python openssh))))

(define-public chameleon+simgrid+nosmpi
  (package
   (inherit chameleon)
   (name "chameleon-simgrid-nosmpi")
   (arguments
    (substitute-keyword-arguments (package-arguments chameleon)
                                  ((#:configure-flags flags '())
                                   `(cons "-DCHAMELEON_SIMULATION=ON" (cons "-DCHAMELEON_USE_CUDA=ON" (delete "-DCHAMELEON_USE_MPI=ON" ,flags))))))
   (inputs (modify-inputs (package-inputs chameleon)
             (prepend simgrid)))
   (propagated-inputs
    (modify-inputs (package-propagated-inputs chameleon)
      (delete "starpu")
      (prepend starpu+simgrid)))))

(define-public chameleon+simgrid
  (package
   (inherit chameleon+simgrid+nosmpi)
   (name "chameleon-simgrid")
   (source
    (origin
      (inherit (package-source chameleon+simgrid+nosmpi))
      (patches (append (origin-patches (package-source chameleon+simgrid+nosmpi))
                       (search-patches "inria/patches/chameleon-simgrid-smpi.patch")))))
   ;; (home-page "https://gitlab.inria.fr/solverstack/chameleon")
   ;; (version "1.1.0")
   ;; (source (origin
   ;;          (method git-fetch)
   ;;          (uri (git-reference
   ;;                (url home-page)
   ;;                (commit "4db899ca30d29927018d83964b9b6d517269abe1")
   ;;                ;; We need the submodule in 'CMakeModules/morse_cmake'.
   ;;                (recursive? #t)))
   ;;          (file-name (string-append name "-" version "-checkout"))
   ;;          (sha256
   ;;           (base32
   ;;            "0mpnacmkn1287c003a6n3c4r0n395l6fnjilzi7z53lb34s8kaap"))
   ;;          (patches (search-patches "inria/patches/chameleon-simgrid-smpi.patch"))))
   (arguments
    (substitute-keyword-arguments (package-arguments chameleon+simgrid+nosmpi)
                                  ((#:configure-flags flags '())
                                   `(delete "-DBUILD_SHARED_LIBS=ON" (cons "-DCHAMELEON_USE_MPI=ON" (cons "-DCMAKE_C_COMPILER=smpicc" (cons "-DCMAKE_CXX_COMPILER=smpicxx" (cons "-DCMAKE_Fortran_COMPILER=smpif90" ,flags))))))
                                  ((#:phases phases '%standard-phases)
                                   `(modify-phases ,phases
                                                   (add-before 'configure 'configure-smpi
                                                               (lambda _
                                                                 ;; https://simgrid.org/doc/latest/app_smpi.html
                                                                 (setenv "SMPI_PRETEND_CC" "1")))
                                                   (add-before 'build 'build-smpi
                                                                (lambda _
                                                                  ;; https://simgrid.org/doc/latest/app_smpi.html
                                                                  (unsetenv "SMPI_PRETEND_CC")))))))))

(define-public chameleon+openmp
  (package
   (inherit chameleon)
   (name "chameleon-openmp")
   (arguments
    (substitute-keyword-arguments (package-arguments chameleon)
                                  ((#:configure-flags flags '())
                                   `(cons "-DCHAMELEON_SCHED=OPENMP" (delete "-DCHAMELEON_USE_MPI=ON" ,flags)))))
   (propagated-inputs
    (modify-inputs (package-propagated-inputs chameleon)
      (delete "starpu" "openmpi")))))

(define-public chameleon+quark
  (package
   (inherit chameleon)
   (name "chameleon-quark")
   (arguments
    (substitute-keyword-arguments (package-arguments chameleon)
                                  ((#:configure-flags flags '())
                                   `(cons "-DCHAMELEON_SCHED=QUARK" (delete "-DCHAMELEON_USE_MPI=ON" ,flags)))))
   (propagated-inputs
    (modify-inputs (package-propagated-inputs chameleon)
      (prepend quark)
      (delete "starpu" "openmpi")))))

(define-public chameleon+parsec
  (package
   (inherit chameleon)
   (name "chameleon-parsec")
   (arguments
    (substitute-keyword-arguments (package-arguments chameleon)
                                  ((#:configure-flags flags '())
                                   `(cons "-DCHAMELEON_SCHED=PARSEC" (delete "-DCHAMELEON_USE_MPI=ON" ,flags)))))
   (propagated-inputs
    (modify-inputs (package-propagated-inputs chameleon)
      (prepend parsec)
      (delete "starpu" "openmpi")))))

(define-public mini-chameleon
  (package
    (inherit chameleon)
    (name "mini-chameleon")
    (version "0.2.0")
    (home-page "https://gitlab.inria.fr/solverstack/mini-examples/mini-chameleon/")
    (synopsis "Educational-purpose dense linear algebra solver")
    (description
     "Mini-chameleon is an educational purpose dense linear algebra solver.
As provided, it essentially provides drivers while the actual computational
routines remain to be completed. The goal is to implement a dense matrix-matrix
product and an LU factorization, first targeting a sequential implementation,
followed by an simd version, a shared-memory openmp one, a distributed memory
MPI one, an MPI+openmp one and a runtime-based starpu one.")
    (license license:cecill-c)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "5428506835e6772bd7f5dc97c5d142943a417433")
                    ;; We need the submodule in 'CMakeModules/morse_cmake'.
                    (recursive? #t)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0hsfpi90i8d3hhbykwgfq4mgsr34ymiahxc8gla9ywhcw8ma59f8"))))
    (arguments
     (substitute-keyword-arguments (package-arguments chameleon)
                                   ((#:configure-flags flags '())
                                    `(cons "-DENABLE_MPI=ON" (cons "-DENABLE_STARPU=ON" (delete "-DCHAMELEON_USE_MPI=ON"
,flags))))))
    (properties '((tunable? . #true)))))

(define-public starpu-example-dgemm
  (package
    (inherit mini-chameleon)
    (name "starpu-example-dgemm")
    (version "0.1.0")
    (home-page "https://gitlab.inria.fr/solverstack/mini-examples/starpu_example_dgemm/")
    (synopsis "StarPU example of a distributed gemm")
    (description
     "Example showing how to use starpu for implementing a distributed gemm.")
    (license license:cecill-c)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "90efe3d1ce2a56253755a5cfb0acbba64975e451")
                    ;; We need the submodule in 'CMakeModules/morse_cmake'.
                    (recursive? #t)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1f8mcg4hcj45cyknb8v5jxba9qzkhimdl6rihf053la30jk72cvd"))))))

(define-public starpu-example-cppgemm
  (package
   (name "starpu-example-cppgemm")
   (version "0.1.0")
   (home-page "https://github.com/Blixodus/starpu_gemm")
   (synopsis "C++ StarPU example of a distributed gemm")
   (description
    "Example showing how to use starpu for implementing a distributed gemm in C++.")
   (license license:cecill-c)
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url home-page)
                  (commit "c9f8467cfc55fc5b7ebcecb1a5b0b7cee501a146")
                  (recursive? #t)))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32
              "0bxs48rqaj0lq36snn798gx6apvnk5mw5jkjqiwdgyclsjfg0c7y"))))
    (build-system cmake-build-system)
    (outputs '("debug" "out"))
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
       #:tests? #f
       #:phases  (modify-phases %standard-phases
                                ;; Allow tests with more MPI processes than available CPU cores,
                                ;; which is not allowed by default by OpenMPI
                                (add-before 'check 'prepare-test-environment
                                            (lambda _
                                              (setenv "OMPI_MCA_rmaps_base_oversubscribe" "1") #t))
                                ;; Some of the tests use StarPU, which expects $HOME
                                ;; to be writable.
                                (add-before 'check 'set-home
                                            (lambda _
                                              (setenv "HOME" (getcwd))
                                              #t)))))
    (inputs (list fmt openblas))
    (propagated-inputs (list starpu openmpi))
    (native-inputs (list pkg-config openssh))))


(define-public maphys
  (package
    (name "maphys")
    (version "1.0.0")
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
                "0pcwfac2x574f6ggfdmahhx9v2hfswyd3nkf3bmc3cd3173312h3"))))
    (build-system cmake-build-system)
    (arguments

     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DMAPHYS_BUILD_TESTS=ON"
                           "-DMAPHYS_SDS_MUMPS=ON"
                           "-DMAPHYS_SDS_PASTIX=ON"
                           "-DCMAKE_EXE_LINKER_FLAGS=-lstdc++"
                           "-DMAPHYS_ITE_FABULOUS=ON"
                           "-DMAPHYS_ORDERING_PADDLE=ON"
                           "-DMAPHYS_BLASMT=ON"
                           )

       #:phases (modify-phases %standard-phases
                  ;; Without this variable, pkg-config removes paths in already in CFLAGS
                  ;; However, gfortran does not check CPATH to find fortran modules
                  ;; and and the module fabulous_mod cannot be found
                  (add-before 'configure 'fix-pkg-config-env
                    (lambda _ (setenv "PKG_CONFIG_ALLOW_SYSTEM_CFLAGS" "1")))
                  (add-before 'configure 'set-fortran-flags
                    (lambda _
                      (define supported-flag?
                        ;; Is '-fallow-argument-mismatch' supported?  It is
                        ;; supported by GCC 10 but not by GCC 7.5.
                        (zero? (system* "gfortran" "-c" "-o" "/tmp/t.o"
                                        "/dev/null" "-fallow-argument-mismatch")))

                      (when supported-flag?
                        (substitute* "CMakeLists.txt"
                          ;; Pass '-fallow-argument-mismatch', which is
                          ;; required when building with GCC 10+.
                          (("-ffree-line-length-0")
                           "-ffree-line-length-0 -fallow-argument-mismatch")))))
                  ;; Allow tests with more MPI processes than available CPU cores,
                  ;; which is not allowed by default by OpenMPI
                  (add-before 'check 'prepare-test-environment
                    (lambda _
                      (setenv "OMPI_MCA_rmaps_base_oversubscribe" "1"))))))

    (inputs (list `(,hwloc "lib")
                  openmpi
                  openssh
                  scalapack
                  openblas
                  ;; ("lapack" ,lapack)
                  pt-scotch-6
                  mumps-openmpi
                  pastix-6.0.3
                  fabulous
                  paddle
                  metis))
    (native-inputs (list gfortran pkg-config))
    (synopsis "Sparse matrix hybrid solver")
    (description
     "MaPHyS (Massively Parallel Hybrid Solver) is a parallel linear solver
that couples direct and iterative approaches. The underlying idea is to
apply to general unstructured linear systems domain decomposition ideas
developed for the solution of linear systems arising from PDEs. The
interface problem, associated with the so called Schur complement system, is
solved using a block preconditioner with overlap between the blocks that is
referred to as Algebraic Additive Schwarz.To cope with the possible lack of
coarse grid mechanism that enables one to keep constant the number of
iterations when the number of blocks is increased, the solver exploits two
levels of parallelism (between the blocks and within the treatment of the
blocks).  This enables it to exploit a large number of processors with a
moderate number of blocks which ensures a reasonable convergence behavior.")
    (license license:cecill-c)))

(define-public paddle
  (package
    (name "paddle")
    (version "0.3.6")
    (home-page "https://gitlab.inria.fr/solverstack/paddle")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "fef4224069c5617366a9fcdfe895514a48acef45")
                    ;; We need the submodule in 'cmake_modules/morse'.
                    (recursive? #t)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0y4csl9r3nr18w1f48k5l83bj0fivjy08fc6njks99qhn3pdwvm1"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DPADDLE_BUILD_TESTS=ON"
                           "-DPADDLE_ORDERING_PARMETIS=OFF")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'change-directory
                    (lambda _ (chdir "src")))
                  (add-before 'configure 'set-fortran-flags
                    (lambda _
                      (define supported-flag?
                        ;; Is '-fallow-argument-mismatch' supported?  It is
                        ;; supported by GCC 10 but not by GCC 7.5.
                        (zero? (system* "gfortran" "-c" "-o" "/tmp/t.o"
                                        "/dev/null" "-fallow-argument-mismatch")))

                      (when supported-flag?
                        (substitute* "CMakeLists.txt"
                          ;; Pass '-fallow-argument-mismatch', which is
                          ;; required when building with GCC 10+.
                          (("-ffree-line-length-none")
                           "-ffree-line-length-none -fallow-argument-mismatch")))))
                  (add-before 'check 'prepare-test-environment
                    (lambda _
                      ;; Allow tests with more MPI processes than available CPU cores,
                      ;; which is not allowed by default by OpenMPI
                      (setenv "OMPI_MCA_rmaps_base_oversubscribe" "1"))))))
    (inputs (list openmpi openssh pt-scotch-6))
    (native-inputs (list gfortran pkg-config))
    (synopsis "Parallel Algebraic Domain Decomposition for Linear systEms")
    (description
     "This  software’s goal is  to propose  a parallel
  algebraic strategy to decompose a  sparse linear system Ax=b, enabling
  its resolution by a domain decomposition solver.  Up to now, Paddle is
  implemented for the MaPHyS linear solver.")
    (license license:cecill-c)))


(define-public fabulous
  (package
    (name "fabulous")
    (version "1.1.2")
    (home-page "https://gitlab.inria.fr/solverstack/fabulous")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    ;; release-1.1.2 branch
                    (commit "5c737d31291ae8dc72983e00d4c05929756e2121")
                    ;; We need the submodule in 'cmake_modules/morse'.
                    (recursive? #t)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1w7gnj9skz8ls9nwy8fn08iw73z95gk8ialblsskfyax801n1j1x"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DFABULOUS_BUILD_C_API=ON"
                           "-DFABULOUS_BUILD_Fortran_API=ON"
                           "-DCMAKE_EXE_LINKER_FLAGS=-lstdc++"
                           "-DFABULOUS_LAPACKE_NANCHECK=OFF"
                           "-DFABULOUS_USE_CHAMELEON=OFF"
                           "-DBUILD_SHARED_LIBS=ON"
                           "-DFABULOUS_BUILD_EXAMPLES=ON"
                           "-DFABULOUS_BUILD_TESTS=OFF")
                         #:tests? #f))
     (inputs (list openblas lapack))
     (native-inputs (list gfortran pkg-config))
     (synopsis "Fast Accurate Block Linear krylOv Solver")
     (description
      "Library implementing Block-GMres with Inexact Breakdown and Deflated Restarting,
Breakdown Free Block Conjudate Gradiant, Block General Conjugate Residual and
Block General Conjugate Residual with Inner Orthogonalization and with inexact breakdown
and deflated restarting")
     (license license:cecill-c)))


(define-public fabulous-1.0.1
  (package
    (inherit fabulous)
    (name "fabulous-1.0.1")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (package-home-page fabulous))
                    (commit "96b3922b981ccc1de4c13bc5341f380f1a72e900")
                    ;; We need the submodule in 'cmake_modules/morse'.
                    (recursive? #t)))
              (file-name (string-append name "-checkout"))
              (sha256
               (base32
                "1nmhr50vhgj8jj4xsd1iswydl4yz1xm4kmyhkbdqvam2nfdjp1y2"))))
     (description
      "Library implementing Block-GMres with Inexact Breakdown and Deflated Restarting,
Breakdown Free Block Conjudate Gradiant, Block General Conjugate Residual.")))

(define maphys++-with-scotch7
  (package
    (name "maphys++-with-scotch7")
    (version "1.1.7")
    (home-page "https://gitlab.inria.fr/solverstack/maphys/maphyspp.git")
    (synopsis "Sparse matrix hybrid solver")
    (description
     "MAPHYS++ is a parallel linear solver for large sparse linear systems.
It implements a modern C++ interface (C++17/20), giving the user a wide range
of solving methods : efficient direct solver (wrapping MUMPS, Pastix...),
iterative solver (CG, GMRES...) and also allowing for a combination of
those (hybrid solve using the Schur complement, with adapted
preconditioners).  Parallelism is based on domain decomposition methods and
is implemented in MPI.")
    (license license:cecill-c)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "68f80bcbbf0e67a764e5cc78ac409f8f5da689b4")
                    ;; We need the submodule in 'cmake_modules/morse_cmake'.
                    (recursive? #t)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1fc59b7nhqyf7gaciq0vc8cp1pqybjh2b7z7fdnphbgcyrw6srqm"))))
    (arguments
     '(#:configure-flags '("-DMAPHYSPP_USE_EIGEN=OFF"
                           "-DMAPHYSPP_USE_FABULOUS=ON"
                           "-DMAPHYSPP_USE_PADDLE=ON"
                           )
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'prepare-test-environment
                    (lambda _
                      ;; Allow tests with more MPI processes than available CPU cores,
                      ;; which is not allowed by default by OpenMPI
                      (setenv "OMPI_MCA_rmaps_base_oversubscribe" "1") #t)))))
    (build-system cmake-build-system)
    (propagated-inputs (list blaspp
                             lapackpp
                             pastix
                             mumps-openmpi
                             arpack-ng-3.8
                             paddle
                             pt-scotch-6 ;; not clear why it must be here
                             fabulous
                             openmpi
                             openssh))
    (native-inputs (list gfortran pkg-config))
    (properties '((tunable? . #true)))))

(define scotch-6-instead-of-scotch-7
  ;; This is a procedure to replace scotch (7) by scotch-6, recursively.
  (package-input-rewriting `((,scotch . ,scotch-6)
                             (,pt-scotch . ,pt-scotch-6))))

(define-public maphys++
  ;; For now Maphys must be built against (pt-)scotch 6.x.
  (package
    (inherit (scotch-6-instead-of-scotch-7 maphys++-with-scotch7))
    (name "maphys++")))

;; Only mpi, blaspp & lapackpp dependencies
(define-public maphys++-minimal
  (package/inherit maphys++
    (name "maphys++-minimal")
    (arguments
     (substitute-keyword-arguments (package-arguments maphys++)
       ((#:configure-flags flags)
        ''("-DMAPHYSPP_USE_PASTIX=OFF"
           "-DMAPHYSPP_USE_MUMPS=OFF"
           "-DMAPHYSPP_USE_EIGEN=OFF"
           "-DMAPHYSPP_USE_ARPACK=OFF"
           "-DMAPHYSPP_DRIVERS=OFF"
           "-DMAPHYSPP_C_DRIVER=OFF"
           "-DMAPHYSPP_Fortran_DRIVER=OFF"
           "-DMAPHYSPP_COMPILE_EXAMPLES=OFF"
           "-DMAPHYSPP_COMPILE_TESTS=ON"))))
    (inputs (fold alist-delete
                  (package-inputs maphys++)
                  '("pastix" "mumps" "arpack" "paddle" "fabulous")))))

;; Minimal + pastix and arpack-ng dependencies
(define-public maphys++-lite
  (package/inherit maphys++
    (name "maphys++-lite")
    (arguments
     (substitute-keyword-arguments (package-arguments maphys++)
       ((#:configure-flags flags)
        ''("-DMAPHYSPP_USE_PASTIX=ON"
           "-DMAPHYSPP_USE_MUMPS=OFF"
           "-DMAPHYSPP_USE_EIGEN=OFF"
           "-DMAPHYSPP_USE_ARPACK=ON"
           "-DMAPHYSPP_DRIVERS=OFF"
           "-DMAPHYSPP_C_DRIVER=OFF"
           "-DMAPHYSPP_Fortran_DRIVER=OFF"
           "-DMAPHYSPP_COMPILE_EXAMPLES=OFF"
           "-DMAPHYSPP_COMPILE_TESTS=ON"))))
    (inputs (fold alist-delete
                  (package-inputs maphys++)
                  '("mumps" "paddle" "fabulous")))))

;; maphys++ with librsb for sparse matrix operations
(define-public maphys++-librsb
  (package/inherit maphys++
                   (name "maphys++-librsb")
                   (arguments
                    (substitute-keyword-arguments (package-arguments maphys++)
                                                  ((#:configure-flags flags '())
                                                   `(cons "-DMAPHYSPP_USE_RSB=ON"
                                                          (cons "-DMAPHYSPP_USE_RSB_SPBLAS=ON" ,flags)))))
                   (inputs `(("librsb" ,librsb)
                             ,@(package-inputs maphys++)))))

(define-public maphys++-eigen
  ;; Variant of Maphys++ that uses Eigen instead of blaspp/lapackpp.
  ;; FIXME: Currently fails to build (blaspp is required at configure time).
  (package/inherit maphys++
    (name "maphys++-eigen")
    (arguments
     (substitute-keyword-arguments (package-arguments maphys++)
       ((#:configure-flags flags)
        ''("-DMAPHYSPP_USE_EIGEN=ON"
           "-DMAPHYSPP_USE_FABULOUS=ON"
           "-DMAPHYSPP_USE_PADDLE=ON"))))
    (inputs
     `(("eigen" ,eigen)
       ,@(fold alist-delete (package-inputs maphys++)
               '("blaspp" "lapackpp"))))))

(define-public blaspp
  (package
    (name "blaspp")
    (version "2021.04.01")
    (home-page "https://bitbucket.org/icl/blaspp")
    (synopsis "C++ API for the Basic Linear Algebra Subroutines")
    (description
     "The Basic Linear Algebra Subprograms (BLAS) have been around for many
decades and serve as the de facto standard for performance-portable and
numerically robust implementation of essential linear algebra functionality.The
objective of BLAS++ is to provide a convenient, performance oriented API for
development in the C++ language, that, for the most part, preserves established
conventions, while, at the same time, takes advantages of modern C++ features,
such as: namespaces, templates, exceptions, etc.")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url home-page)
                   (commit "314bafceead689a35aab826e03aa76bf329cfb0e")))
             (file-name (string-append name "-" version "-checkout"))
             (sha256
              (base32
               "0n57c02jcd2kmw9zldyhvxp80xgy1gmmaccy1sr6g5nnp3jl175m"))))
    (arguments
     '(#:configure-flags '("-Dbuild_tests=OFF")
                         #:tests? #f))
    ;; tests would need testsweeper https://bitbucket.org/icl/testsweeper
    ;;'(#:configure-flags '("-DBLASPP_BUILD_TESTS=ON")))
    (build-system cmake-build-system)
    (propagated-inputs (list openblas)) ;; technically only blas
    (license license:bsd-3)))

(define-public lapackpp
  (package
   (name "lapackpp")
   (version "2021.04.00")
   (home-page "https://bitbucket.org/icl/lapackpp")
   (synopsis "C++ API for the Linear Algebra PACKage")
   (description
    "The Linear Algebra PACKage (LAPACK) is a standard software library for
numerical linear algebra. The objective of LAPACK++ is to provide a convenient,
performance oriented API for development in the C++ language, that, for the most
part, preserves established conventions, while, at the same time, takes
advantages of modern C++ features, such as: namespaces, templates, exceptions,
etc.")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url home-page)
                  (commit "31d969200a9f65390f56ac2ea48888bd10a13397")))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32
              "06xipc9j9xgh5rk1fxxgpk1gla5mjq2l0511q6487zd81720ka7w"))))
   (arguments
    '(#:configure-flags '("-DBUILD_LAPACKPP_TESTS=OFF"
                          "-Dbuild_tests=OFF")
                        #:tests? #f))
    ;; tests would need testsweeper https://bitbucket.org/icl/testsweeper
    (build-system cmake-build-system)
    (inputs (list blaspp))
    (license license:bsd-3)))

(define-public pastix-6
  (package
    (name "pastix")
    (version "6.2.2")
    (home-page "https://gitlab.inria.fr/solverstack/pastix")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "6e35c2ba179013b058f5d4e1afd4b451953149c6")
                    ;; We need the submodule in 'cmake_modules/morse'.
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bv2lsbkwjbmz4knm08g6sds4irk0g1bm9m0h1scab0yxx2izcka"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DPASTIX_WITH_MPI=ON"
                           "-DPASTIX_WITH_PARSEC=ON"
                           "-DPASTIX_WITH_STARPU=ON"
                           "-DPASTIX_ORDERING_METIS=OFF"
                           "-DPASTIX_ORDERING_SCOTCH=ON"
                           "-DCMAKE_C_FLAGS=-fcommon")

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

                        ;; Allow the 'python_simple' test to find spm.py.
                        (setenv "PYTHONPATH"
                                (string-append (getcwd) "/../source"
                                               "/spm/wrappers/python/spm:"
                                               (or (getenv "GUIX_PYTHONPATH")
                                                   (getenv "PYTHONPATH"))))))))

       ;; XXX: The 'python_simple' test fails with:
       ;;   ValueError: Attempted relative import in non-package
       #:tests? #f))
    (native-inputs
     (list pkg-config gfortran))
    (inputs
     (list `(,gfortran "lib") ;for 'gcc … -lgfortran'
           openblas
           ;; ("lapack" ,lapack)         ;must be built with '-DLAPACKE_WITH_TMG=ON'

           ;; Python bindings and Python tests. Python3
           python

           python-numpy
           ;;("python-scipy" ,python-scipy)
           ))
    (propagated-inputs (list `(,hwloc "lib")
                             scotch

                             ;; The following are optional dependencies.
                             ;;GM: somehow these two are needed in propagated-inputs
                             ;;in order to compile maphys++ (otherwise cmake fails
                             ;;to find them)
                             parsec+mpi
                             starpu))
    (synopsis "Sparse matrix direct solver")
    (description
     "PaStiX (Parallel Sparse matriX package) is a scientific library that
provides a high performance parallel solver for very large sparse linear
systems based on direct methods.  Numerical algorithms are implemented in
single or double precision (real or complex) using LLt, LDLt and LU with
static pivoting (for non symmetric matrices having a symmetric pattern).
This solver also provides some low-rank compression methods to reduce the
memory footprint and/or the time-to-solution.")
    (license license:cecill)))

(define-public pastix-6.0.3
  (package
    (name "pastix")
    (version "6.0.3")
    (home-page "https://gitlab.inria.fr/solverstack/pastix")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))

                    ;; We need the submodule in 'cmake_modules/morse'.
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ccj5p1zm50x3qd6yr7j5ajh2dlpmm95lhzrbyv9rbnxrk66azid"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DPASTIX_WITH_MPI=ON"
                           "-DPASTIX_WITH_PARSEC=ON"
                           "-DPASTIX_WITH_STARPU=ON"
                           "-DPASTIX_ORDERING_METIS=OFF"
                           "-DPASTIX_ORDERING_SCOTCH=ON"
                           "-DCMAKE_C_FLAGS=-fcommon")

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

                        ;; Allow the 'python_simple' test to find spm.py.
                        (setenv "PYTHONPATH"
                                (string-append (getcwd) "/../source"
                                               "/spm/wrappers/python/spm:"
                                               (or (getenv "GUIX_PYTHONPATH")
                                                   (getenv "PYTHONPATH"))))
                        #t))))

       ;; XXX: The 'python_simple' test fails with:
       ;;   ValueError: Attempted relative import in non-package
       #:tests? #f))
    (native-inputs
     (list pkg-config gfortran))
    (inputs
     (list `(,gfortran "lib") ;for 'gcc … -lgfortran'
           openblas
           ;; ("lapack" ,lapack)         ;must be built with '-DLAPACKE_WITH_TMG=ON'

           ;; The following are optional dependencies.
           parsec+mpi
           starpu

           ;; Python bindings and Python tests. Python3
           python

           python-numpy
           ;;("python-scipy" ,python-scipy)
           ))
    (propagated-inputs (list `(,hwloc "lib") scotch))
    (synopsis "Sparse matrix direct solver")
    (description
     "PaStiX (Parallel Sparse matriX package) is a scientific library that
provides a high performance parallel solver for very large sparse linear
systems based on direct methods.  Numerical algorithms are implemented in
single or double precision (real or complex) using LLt, LDLt and LU with
static pivoting (for non symmetric matrices having a symmetric pattern).
This solver also provides some low-rank compression methods to reduce the
memory footprint and/or the time-to-solution.")
    (license license:cecill)))

(define-public pastix-nompi
  (package
   (inherit pastix-6)
   (name "pastix-nompi")
   (arguments
    (substitute-keyword-arguments (package-arguments pastix-6)
                                  ((#:configure-flags flags '())
                                   `(delete "-DPASTIX_WITH_MPI=ON" ,flags))))
   (inputs `(,@(delete `("parsec" ,parsec+mpi) (package-inputs pastix-6))
             ("parsec" ,parsec)
             ,@(package-inputs pastix-6)))))

(define-public pastix-5
  (package
  (name "pastix-5")
  (version "5.2.3")
  (home-page "https://gitlab.inria.fr/solverstack/pastix")
  (source
   (origin
    (method url-fetch)
    (uri
     "https://gforge.inria.fr/frs/download.php/file/36212/pastix_5.2.3.tar.bz2")
    (sha256
     (base32
      "0iqyxr5lzjpavmxzrjj4kwayq62nip3ssjcm80d20zk0n3k7h6b4"))))
  (build-system gnu-build-system)
  (arguments
   '(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
     #:phases
     (modify-phases %standard-phases
                    (add-after 'unpack 'goto-src-dir
                             (lambda _
                               (chdir "src") #t))
                    (replace 'configure
                             (lambda* (#:key inputs #:allow-other-keys)
                               (call-with-output-file "config.in"
                                 (lambda (port)
                                   (format port "
HOSTARCH    = i686_pc_linux
VERSIONBIT  = _32bit
EXEEXT      =
OBJEXT      = .o
LIBEXT      = .a
CCPROG      = gcc -Wall
CFPROG      = gfortran
CF90PROG    = gfortran -ffree-form
CXXPROG     = g++

MCFPROG     = mpif90
CF90CCPOPT  = -ffree-form -x f95-cpp-input -fallow-argument-mismatch
# Compilation options for optimization (make expor)
CCFOPT      = -O3
# Compilation options for debug (make | make debug)
CCFDEB      = -g3
CXXOPT      = -O3
NVCCOPT     = -O3

LKFOPT      =
MKPROG      = make
MPCCPROG    = mpicc -Wall
MPCXXPROG   = mpic++ -Wall
CPP         = cpp
ARFLAGS     = ruv
ARPROG      = ar
EXTRALIB    = -lgfortran -lm -lrt
CTAGSPROG   = ctags

VERSIONMPI  = _mpi
VERSIONSMP  = _smp
VERSIONSCH  = _static
VERSIONINT  = _int
VERSIONPRC  = _simple
VERSIONFLT  = _real
VERSIONORD  = _scotch

###################################################################
#                  SETTING INSTALL DIRECTORIES                    #
###################################################################
# ROOT          = /path/to/install/directory
# INCLUDEDIR    = ${ROOT}/include
# LIBDIR        = ${ROOT}/lib
# BINDIR        = ${ROOT}/bin
# PYTHON_PREFIX = ${ROOT}

###################################################################
#                  SHARED LIBRARY GENERATION                      #
###################################################################
SHARED=1
SOEXT=.so
SHARED_FLAGS =  -shared -Wl,-soname,__SO_NAME__
CCFDEB       := ${CCFDEB} -fPIC
CCFOPT       := ${CCFOPT} -fPIC
CFPROG       := ${CFPROG} -fPIC

###################################################################
#                          INTEGER TYPE                           #
###################################################################
# Uncomment the following lines for integer type support (Only 1)

#VERSIONINT  = _long
#CCTYPES     = -DFORCE_LONG -DINTSIZELONG
#---------------------------
VERSIONINT  = _int32
CCTYPES     = -DINTSIZE32
#---------------------------
#VERSIONINT  = _int64
#CCTYPES     = -DINTSSIZE64

###################################################################
#                           FLOAT TYPE                            #
###################################################################
CCTYPESFLT  =
# Uncomment the following lines for double precision support
VERSIONPRC  = _double
CCTYPESFLT := $(CCTYPESFLT) -DPREC_DOUBLE

# Uncomment the following lines for float=complex support
VERSIONFLT  = _complex
CCTYPESFLT := $(CCTYPESFLT) -DTYPE_COMPLEX

###################################################################
#                          FORTRAN MANGLING                       #
###################################################################
#CCTYPES    := $(CCTYPES) -DPASTIX_FM_NOCHANGE

###################################################################
#                          MPI/THREADS                            #
###################################################################

# Uncomment the following lines for sequential (NOMPI) version
#VERSIONMPI  = _nompi
#CCTYPES    := $(CCTYPES) -DFORCE_NOMPI
#MPCCPROG    = $(CCPROG)
#MCFPROG     = $(CFPROG)
#MPCXXPROG   = $(CXXPROG)

# Uncomment the following lines for non-threaded (NOSMP) version
#VERSIONSMP  = _nosmp
#CCTYPES    := $(CCTYPES) -DFORCE_NOSMP

# Uncomment the following line to enable a progression thread,
#  then use IPARM_THREAD_COMM_MODE
#CCPASTIX   := $(CCPASTIX) -DPASTIX_THREAD_COMM

# Uncomment the following line if your MPI doesn't support MPI_THREAD_MULTIPLE,
# level then use IPARM_THREAD_COMM_MODE
#CCPASTIX   := $(CCPASTIX) -DPASTIX_FUNNELED

# Uncomment the following line if your MPI doesn't support MPI_Datatype
# correctly
#CCPASTIX   := $(CCPASTIX) -DNO_MPI_TYPE

# Uncomment the following line if you want to use semaphore barrier
# instead of MPI barrier (with IPARM_AUTOSPLIT_COMM)
#CCPASTIX    := $(CCPASTIX) -DWITH_SEM_BARRIER

# Uncomment the following lines to enable StarPU.
#CCPASTIX   := $(CCPASTIX) `pkg-config libstarpu --cflags` -DWITH_STARPU
#EXTRALIB   := $(EXTRALIB) `pkg-config libstarpu --libs`
# Uncomment the correct 2 lines
#CCPASTIX   := $(CCPASTIX) -DCUDA_SM_VERSION=11
#NVCCOPT    := $(NVCCOPT) -maxrregcount 32 -arch sm_11
#CCPASTIX   := $(CCPASTIX) -DCUDA_SM_VERSION=13
#NVCCOPT    := $(NVCCOPT) -maxrregcount 32 -arch sm_13
CCPASTIX   := $(CCPASTIX) -DCUDA_SM_VERSION=20
NVCCOPT    := $(NVCCOPT) -arch sm_20

# Uncomment the following line to enable StarPU profiling
# ( IPARM_VERBOSE > API_VERBOSE_NO ).
#CCPASTIX   := $(CCPASTIX) -DSTARPU_PROFILING

# Uncomment the following line to enable CUDA (StarPU)
#CCPASTIX   := $(CCPASTIX) -DWITH_CUDA

###################################################################
#                          Options                                #
###################################################################

# Show memory usage statistics
CCPASTIX   := $(CCPASTIX) -DMEMORY_USAGE

# Show memory usage statistics in solver
#CCPASTIX   := $(CCPASTIX) -DSTATS_SOPALIN

# Uncomment following line for dynamic thread scheduling support
#CCPASTIX   := $(CCPASTIX) -DPASTIX_DYNSCHED

# Uncomment the following lines for Out-of-core
#CCPASTIX   := $(CCPASTIX) -DOOC -DOOC_NOCOEFINIT -DOOC_DETECT_DEADLOCKS

###################################################################
#                      GRAPH PARTITIONING                         #
###################################################################

# Uncomment the following lines for using metis ordering
#VERSIONORD  = _metis
#METIS_HOME  = ${HOME}/metis-4.0
#CCPASTIX   := $(CCPASTIX) -DMETIS -I$(METIS_HOME)/Lib
#EXTRALIB   := $(EXTRALIB) -L$(METIS_HOME) -lmetis

# Scotch always needed to compile
SCOTCH_HOME  = ~a
SCOTCH_INC  ?= $(SCOTCH_HOME)/include
SCOTCH_LIB  ?= $(SCOTCH_HOME)/lib
# Uncomment on of this blocks
#scotch
CCPASTIX   := $(CCPASTIX) -I$(SCOTCH_INC) -DWITH_SCOTCH
EXTRALIB   := $(EXTRALIB) -L$(SCOTCH_LIB) -lscotch -lscotcherrexit
#ptscotch
#CCPASTIX   := $(CCPASTIX) -I$(SCOTCH_INC) -DDISTRIBUTED -DWITH_SCOTCH
#if scotch >= 6.0
#EXTRALIB   := $(EXTRALIB) -L$(SCOTCH_LIB) -lptscotch -lscotch -lptscotcherrexit
#else
#EXTRALIB   := $(EXTRALIB) -L$(SCOTCH_LIB) -lptscotch -lptscotcherrexit

###################################################################
#                Portable Hardware Locality                       #
###################################################################
# By default PaStiX uses hwloc to bind threads,
# comment this lines if you don't want it (not recommended)
HWLOC_HOME  = ~a
HWLOC_INC  ?= $(HWLOC_HOME)/include
HWLOC_LIB  ?= $(HWLOC_HOME)/lib
CCPASTIX   := $(CCPASTIX) -I$(HWLOC_INC) -DWITH_HWLOC
EXTRALIB   := $(EXTRALIB) -L$(HWLOC_LIB) -lhwloc

###################################################################
#                             MARCEL                              #
###################################################################

# Uncomment following lines for marcel thread support
#VERSIONSMP := $(VERSIONSMP)_marcel
#CCPASTIX   := $(CCPASTIX) `pm2-config --cflags`
#CCPASTIX   := -I${PM2_ROOT}/marcel/include/pthread
#EXTRALIB   := $(EXTRALIB) `pm2-config --libs`
# ---- Thread Posix ------
EXTRALIB   := $(EXTRALIB) -lpthread

# Uncomment following line for bubblesched framework support
# (need marcel support)
#VERSIONSCH  = _dyn
#CCPASTIX   := $(CCPASTIX) -DPASTIX_BUBBLESCHED

###################################################################
#                              BLAS                               #
###################################################################

# Choose Blas library (Only 1)
# Do not forget to set BLAS_HOME if it is not in your environnement
# BLAS_HOME=/path/to/blas
#----  Blas    ----
BLASLIB  = -lopenblas
#---- Gotoblas ----
#BLASLIB  = -L${BLAS_HOME} -lgoto
#----  MKL     ----
#Uncomment the correct line
#BLASLIB  = -L$(BLAS_HOME) -lmkl_intel_lp64 -lmkl_sequential -lmkl_core
#BLASLIB  = -L$(BLAS_HOME) -lmkl_intel -lmkl_sequential -lmkl_core
#----  Acml    ----
#BLASLIB  = -L$(BLAS_HOME) -lacml

###################################################################
#                             MURGE                               #
###################################################################
# Uncomment if you need MURGE interface to be thread safe
# CCPASTIX   := $(CCPASTIX) -DMURGE_THREADSAFE
# Uncomment this to have more timings inside MURGE
# CCPASTIX   := $(CCPASTIX) -DMURGE_TIME

###################################################################
#                          DO NOT TOUCH                           #
###################################################################

FOPT      := $(CCFOPT)
FDEB      := $(CCFDEB)
CCHEAD    := $(CCPROG) $(CCTYPES) $(CCFOPT)
CCFOPT    := $(CCFOPT) $(CCTYPES) $(CCPASTIX)
CCFDEB    := $(CCFDEB) $(CCTYPES) $(CCPASTIX)
NVCCOPT   := $(NVCCOPT) $(CCTYPES) $(CCPASTIX)

###################################################################
#                        MURGE COMPATIBILITY                      #
###################################################################

MAKE     = $(MKPROG)
CC       = $(MPCCPROG)
CFLAGS   = $(CCFOPT) $(CCTYPESFLT)
FC       = $(MCFPROG)
FFLAGS   = $(CCFOPT)
LDFLAGS  = $(EXTRALIB) $(BLASLIB)
CTAGS    = $(CTAGSPROG)
"
                                           (assoc-ref inputs "scotch32")
                                           (assoc-ref inputs "hwloc")
                                   ))) #t))
                    (replace 'check
                             (lambda _
                               (invoke "make" "examples")
                               (invoke "./example/bin/simple" "-lap" "100"))))))
  (inputs
   (list `(,gfortran "lib") openblas))
  (native-inputs
   (list pkg-config gfortran perl))
  (propagated-inputs
   (list openmpi-with-mpi1-compat
         `(,hwloc-1 "lib") openssh scotch32))
  (outputs '( "out" "debug" ))
  (synopsis "Sparse matrix direct solver (version 5)")
  (description
   "PaStiX (Parallel Sparse matriX package, version 5) is a scientific library
that provides a high performance parallel solver for very large sparse linear
systems based on direct methods.  Numerical algorithms are implemented in single
or double precision (real or complex) using LLt, LDLt and LU with static
pivoting (for non symmetric matrices having a symmetric pattern). This solver
also provides some low-rank compression methods to reduce the memory footprint
and/or the time-to-solution.")
  (license license:cecill)))

(define-public pastix pastix-6)

(define-public pastix-nopython-notest
  (package
   (inherit pastix)
   (name "pastix-nopython-notest")
   (arguments
    (substitute-keyword-arguments (package-arguments chameleon)
                                  ((#:configure-flags flags '())
                                   `(cons "-DPASTIX_BUILD_TESTING=OFF" ,flags))))))

(define-public pmtool
  (package
    (name "pmtool")
    (version "1.0.0")
    (home-page "https://gitlab.inria.fr/eyrauddu/pmtool")
    (synopsis "pmtool: Post-Mortem Tool")
    (description
     "pmtool aims at performing post-mortem analyses of the behavior
of StarPU applications. Provide lower bounds on makespan. Study the
performance of different schedulers in a simple context. Limitations:
ignore communications for the moment; branch comms attempts to remove
this limitation.")
    (license license:gpl3+)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "e11dc2996ab976275ac678c2686db2afcf480137")
                    (recursive? #f)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "18dad0w21av8jps8mfzj99mb237lrr27mmgbh3s16g94lfpnffd9"))))
    (build-system cmake-build-system)
    (outputs '("debug" "out"))
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=OFF")

       ;; FIXME: no make test available for now in pmtool
       #:tests? #f))

    (inputs (list recutils))
    (native-inputs (list gcc-toolchain))))

(define-public scalable-python
  (package
   (inherit python-2.7)
   (name "scalable-python")
   (version "2.7.13")
   (home-page "https://github.com/CSCfi/scalable-python.git")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url home-page)
                  (commit "b0b9d3f29298b719f9e4f684deae713c0a224b0e")
                  ))
            (patches (search-patches
                      "inria/patches/scalable-python.patch"
                      "python-2.7-search-paths.patch"
                      "python-2-deterministic-build-info.patch"
                      "python-2.7-site-prefixes.patch"
                      ))
            (sha256
             (base32
              "0ivxsf17x7vjxr5h4g20rb5i3k705vgd502ma024z95fnyzd0bqi"))))
   (arguments
    (substitute-keyword-arguments (package-arguments python-2.7)
                                  ((#:phases phases)
                                   `(modify-phases ,phases
                                                   (add-before 'configure 'permissions_gramfiles
                                                               (lambda _
                                                                 (chmod "Python/graminit.c" #o764)
                                                                 (chmod "Include/graminit.h" #o764) #t))
                                                   (add-before 'build 'fix_makefile
                                                               (lambda _
                                                                 (chmod "Python/graminit.c" #o764)
                                                                 (chmod "Include/graminit.h" #o764) #t))
                                                   (replace 'move-tk-inter (lambda _ #t)) ;; Not sure what this is anyway
                                                   ))
                                  ((#:configure-flags flags '())
                                   `(cons "--enable-mpi" (cons "--without-ensurepip" (delete "--with-ensurepip=install",flags))))
                                  ((#:make-flags makeflags '())
                                   `(cons "mpi" (cons "install" (cons "install-mpi",makeflags))))
                                  ((#:tests? runtests '())
                                   #f)
                                  ))
   (description
    "Modified python 2.7.13. Scalable Python performs the I/O operations used
e.g. by import statements in a single process and uses MPI to transmit data
to/from all other processes.")
   (propagated-inputs (list openmpi))
   ))

;; Fix python2-sympy
(define-public fixed-python2-sympy
  (package
   (inherit (package-with-python2 python-sympy))
   (name "fixed-python2-sympy")
   (arguments
    `(#:python ,python-2
               #:phases
               (modify-phases %standard-phases
                              ;; Run the core tests after installation.  By default it would run
                              ;; *all* tests, which take a very long time to complete and are known
                              ;; to be flaky.
                              (delete 'check)
                              (add-after 'install 'check
                                         (lambda* (#:key outputs #:allow-other-keys)
                                                  (invoke "python" "-c" "import sympy; sympy.test(\"/core\")")
                                                  #t)))))))
;; Add mpi4py with python2
(define-public python2-mpi4py
  (package-with-python2 python-mpi4py))

(define-public arpack-ng-3.8
  (package
   (name "arpack-ng-3.8")
   (version "3.8.0")
   (home-page "https://github.com/opencollab/arpack-ng")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url home-page) (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0l7as5z6xvbxly8alam9s4kws70952qq35a6vkljzayi4b9gbklx"))))
   (build-system cmake-build-system)
   (arguments
    '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                          "-DICB=ON")))
   (inputs
    (list lapack gfortran))
   (synopsis "Fortran subroutines for solving eigenvalue problems")
   (description
    "ARPACK-NG is a collection of Fortran77 subroutines designed to solve
large scale eigenvalue problems.")
   (license (license:non-copyleft "file://COPYING"
                                  "See COPYING in the distribution."))))

;; Technically this is just jube version 2.4.1
;; supporting yaml (in addition of xml) for bench files from version 2.4.0
(define-public jube-with-yaml
  (package
    ;; This is a command-line tool, so no "python-" prefix.
   (name "jube-with-yaml")
    (version "2.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://apps.fz-juelich.de/jsc/jube/jube2/download.php?version="
                    version))
              (sha256
               (base32
                "05lhpq3mxm3z9k35bxvvbi2r5r5r0n39jdi839rcvirrsczs7m6m"))
              (file-name (string-append "jube-" version ".tar.gz"))))
    (build-system python-build-system)
    (home-page "https://apps.fz-juelich.de/jsc/jube/jube2/docu/index.html")
    (synopsis "Benchmarking environment")
    (description
     "JUBE helps perform and analyze benchmarks in a systematic way.  For each
benchmarked application, benchmark data is stored in a format that allows JUBE
to deduct the desired information.  This data can be parsed by automatic pre-
and post-processing scripts that draw information and store it more densely
for manual interpretation.")
    (propagated-inputs (list python-pyyaml))
    (license license:gpl3+)))

(define-public scalfmm
  (package
   (name "scalfmm")
   (version "3.0")
   (home-page "https://gitlab.inria.fr/solverstack/ScalFMM.git")
   (synopsis "Fast Multipole Methos Framework")
   (description
    "ScalFMM is a C++ library that implements a kernel independent Fast Multipole Method.")
   (license license:cecill-c)
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url home-page)
                  (commit "33ea9ae3f0c39b31fa9fb631653e34340d8100dd")
                  ;; We need the submodule in 'cmake_modules/morse_cmake'.
                  (recursive? #t)))
            (file-name (string-append name "-" version))
            (sha256
             (base32
              "05n7isr54zynw6gf6mgc973qdg8nm06f434y3l3qvwz2ijjvb43f"))))
   (arguments
    '(#:configure-flags '("-Dscalfmm_BUILD_EXAMPLES=ON"
                          "-Dscalfmm_BUILD_TOOLS=ON"
                          "-Dscalfmm_BUILD_UNITS=ON")
                        #:tests? #f
                        #:phases (modify-phases %standard-phases
                                    (add-after 'unpack 'goto-src-dir
                                        (lambda _
                                            (chdir "experimental") #t))
                                    (add-before 'check 'prepare-test-environment
                                        (lambda _
                                          ;; Allow tests with more MPI processes than available CPU cores,
                                          ;; which is not allowed by default by OpenMPI
                                          (setenv "OMPI_MCA_rmaps_base_oversubscribe" "1") #t)))
                        ))
   (build-system cmake-build-system)
   (inputs (list openblas fftw fftwf))
   (propagated-inputs (list openmpi openssh))
   (native-inputs (list pkg-config))
   (properties '((tunable? . #true)))))

(define-public ddmpy
  (package
   (name "ddmpy")
   (version "0.1")
   (synopsis "DDMPY: a Domain Decomposition Methods PYthon package")
   (home-page "https://gitlab.inria.fr/compose/ddmpy.git")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url home-page)
                  (commit "66dc3cc79dfdc5864fc62aa371173c8a576cc0ae")))
            (sha256
             (base32
              "0xbw4zkd5bk0m4mj0snxw19ig0yy5fvj5lphs0hwhq44cdxm77s6"))))
   (build-system python-build-system)
   (propagated-inputs (list openmpi
                            openssh
                            python
                            python-numpy
                            python-scipy
                            python-mpi4py))
    (description
     "Linear algebra package implementing advanced parallel domain decomposition
methods.")
  (license license:cecill-c)))
