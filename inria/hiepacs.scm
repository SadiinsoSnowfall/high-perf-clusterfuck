;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019 Inria

(define-module (inria hiepacs)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
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
  #:use-module (inria mpi)
  #:use-module (inria storm)
  #:use-module (inria tadaam)
  #:use-module (inria eztrace)
  #:use-module (inria simgrid)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  ;; To remove when/if python2 packages sympy and mpi4py
  ;; are fixed in official repo
  #:use-module (guix build-system python)
  )

(define-public parsec
  (package
    (name "parsec")
    (version "210d9d2b8cb292b64c01a597047581f27cf8cb54")
    (home-page "https://bitbucket.org/mfaverge/parsec.git")
    (synopsis "Runtime system based on dynamic task generation mechanism")
    (description
     "PaRSEC is a generic framework for architecture aware scheduling
and management of micro-tasks on distributed many-core heterogeneous
architectures.")
    (license license:bsd-2)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "210d9d2b8cb292b64c01a597047581f27cf8cb54")
                    (recursive? #t)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1yfny4ra9v3nxal1mbi0jqpivc0qamrx7m270qqnxjjp95vvky4z"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DPARSEC_GPU_WITH_CUDA=OFF"
                           "-DPARSEC_DIST_WITH_MPI=OFF")
       #:tests? #f))
    (inputs `(("hwloc" ,hwloc)
              ("bison" ,bison)
              ("flex" ,flex)))
    (native-inputs `(("gfortran" ,gfortran)
                     ("python" ,python-2)))))

(define-public quark
  (package
    (name "quark")
    (version "db4aef9a66a00487d849cf8591927dcebe18ef2f")
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
              (uri (git-reference
                    (url home-page)
                    (commit "db4aef9a66a00487d849cf8591927dcebe18ef2f")))
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
    (inputs `(("hwloc" ,hwloc)))
    (native-inputs `(("gfortran" ,gfortran)))))

(define-public chameleon
  (package
    (name "chameleon")
    (version "1.0.0")
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
                    (commit "da81c248e229dc879b06bec41b1f5468416b7d9a")
                    ;; We need the submodule in 'CMakeModules/morse_cmake'.
                    (recursive? #t)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "13yz75yqd6ilnw4s5ksb5qbvp18v2q4kgwq8w9ic8lfcw96p1bh9"))))
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
    (inputs `(("lapack" ,openblas)))
    (propagated-inputs `(("starpu" ,starpu)
                         ("mpi" ,openmpi)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("gfortran" ,gfortran)
                     ("python" ,python)
                     ("openssh" ,openssh)))))

(define-public chameleon+simgrid
  (package
   (inherit chameleon)
   (name "chameleon-simgrid")
   (arguments
    (substitute-keyword-arguments (package-arguments chameleon)
                                  ((#:configure-flags flags '())
                                   `(cons "-DCHAMELEON_SIMULATION=ON" (cons "-DCHAMELEON_USE_CUDA=ON" (delete "-DCHAMELEON_USE_MPI=ON" ,flags))))))
   (inputs `(("simgrid" ,simgrid)
             ,@(package-inputs chameleon)))
   (propagated-inputs `(("starpu" ,starpu+simgrid+fxt)
                        ,@(delete `("starpu" ,starpu) (package-inputs chameleon))
                        ,@(delete `("mpi" ,openmpi) (package-inputs chameleon))))))

(define-public chameleon+openmp
  (package
   (inherit chameleon)
   (name "chameleon-openmp")
   (arguments
    (substitute-keyword-arguments (package-arguments chameleon)
                                  ((#:configure-flags flags '())
                                   `(cons "-DCHAMELEON_SCHED=OPENMP" (delete "-DCHAMELEON_USE_MPI=ON" ,flags)))))
   (propagated-inputs `(,@(delete `("starpu" ,starpu) (package-inputs chameleon))
                        ,@(delete `("mpi" ,openmpi) (package-inputs chameleon))))))

(define-public chameleon+quark
  (package
   (inherit chameleon)
   (name "chameleon-quark")
   (arguments
    (substitute-keyword-arguments (package-arguments chameleon)
                                  ((#:configure-flags flags '())
                                   `(cons "-DCHAMELEON_SCHED=QUARK" (delete "-DCHAMELEON_USE_MPI=ON" ,flags)))))
   (propagated-inputs `(("quark" ,quark)
             ,@(delete `("starpu" ,starpu) (package-inputs chameleon))
             ,@(delete `("mpi" ,openmpi) (package-inputs chameleon))))))

(define-public chameleon+parsec
  (package
   (inherit chameleon)
   (name "chameleon-parsec")
   (arguments
    (substitute-keyword-arguments (package-arguments chameleon)
                                  ((#:configure-flags flags '())
                                   `(cons "-DCHAMELEON_SCHED=PARSEC" (delete "-DCHAMELEON_USE_MPI=ON" ,flags)))))
   (propagated-inputs `(("parsec" ,parsec)
             ,@(delete `("starpu" ,starpu) (package-inputs chameleon))
             ,@(delete `("mpi" ,openmpi) (package-inputs chameleon))))))

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
                                           (lambda _ (setenv "PKG_CONFIG_ALLOW_SYSTEM_CFLAGS" "1") #t))
                               ;; Allow tests with more MPI processes than available CPU cores,
                               ;; which is not allowed by default by OpenMPI
                               (add-before 'check 'prepare-test-environment
                                           (lambda _
                                             (setenv "OMPI_MCA_rmaps_base_oversubscribe" "1") #t)))))

    (inputs `(("hwloc" ,hwloc "lib")
              ("openmpi" ,openmpi)
              ("ssh" ,openssh)
              ("scalapack" ,scalapack)
              ("openblas" ,openblas)
              ;; ("lapack" ,lapack)
              ("scotch" ,pt-scotch)
              ("mumps" ,mumps-openmpi)
              ("pastix" ,pastix)
              ("fabulous" ,fabulous)
              ("paddle", paddle)
              ("metis" ,metis)))
    (native-inputs `(("gfortran" ,gfortran)
                     ("pkg-config" ,pkg-config)))
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
    (version "0.3.4")
    (home-page "https://gitlab.inria.fr/solverstack/paddle")
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
                "0znb88zi739z4lr2w116810gblw1hzd6pimsrmwivjvr94a0655i"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DPADDLE_BUILD_TESTS=ON"
                           "-DPADDLE_ORDERING_PARMETIS=OFF")
       #:phases (modify-phases %standard-phases
                                (add-before 'configure 'change-directory
                                            (lambda _ (chdir "src") #t))
                                (add-before 'check 'prepare-test-environment
                                            (lambda _
                                              ;; Allow tests with more MPI processes than available CPU cores,
                                              ;; which is not allowed by default by OpenMPI
                                              (setenv "OMPI_MCA_rmaps_base_oversubscribe" "1") #t)))))
    (inputs `(("openmpi" ,openmpi)
              ("ssh" ,openssh)
              ("scotch" ,pt-scotch)))
    (native-inputs `(("gfortran" ,gfortran)
                     ("pkg-config" ,pkg-config)))
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
    (version "1.0.1")
    (home-page "https://gitlab.inria.fr/solverstack/fabulous")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "96b3922b981ccc1de4c13bc5341f380f1a72e900")
                    ;; We need the submodule in 'cmake_modules/morse'.
                    (recursive? #t)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1nmhr50vhgj8jj4xsd1iswydl4yz1xm4kmyhkbdqvam2nfdjp1y2"))))
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
     (inputs `(("openblas" ,openblas)
               ("lapack" ,lapack)))
     (native-inputs `(("gfortran" ,gfortran)
                      ("pkg-config" ,pkg-config)))
     (synopsis "Fast Accurate Block Linear krylOv Solver")
     (description
      "Library implementing Block-GMres with Inexact Breakdown and Deflated Restarting")
     (license license:cecill-c)))

(define-public maphys++
  (package
   (name "maphys++")
   (version "0.1")
   (home-page "https://gitlab.inria.fr/solverstack/maphys/maphyspp.git")
   (synopsis "Sparse matrix hybrid solver")
   (description
    "MaPHyS (Massively Parallel Hybrid Solver) is a parallel linear solver
that couples direct and iterative approaches. The underlying idea is to
apply to general unstructured linear systems domain decomposition ideas
developed for the solution of linear systems arising from PDEs.
This new implementation in C++ offers a wide range of hybrid methods to
solve massive sparse systems efficiently.")
   (license license:cecill-c)
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url home-page)
                  (commit "14ce2b79d227d5cddb908eac018ed3d7efee734e")
                  ;; We need the submodule in 'cmake_modules/morse_cmake'.
                  (recursive? #t)))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32
              "03sjykh24ms4h2vzylkxcc6v7nshl3w0dhyyrv9grzckmxvmvzij"))))
   (arguments
    '(#:configure-flags '("-DMAPHYS_COMPILE_EXAMPLES=ON"
                          "-DMAPHYS_COMPILE_TESTS=ON"
                          "-DMAPHYS_USE_ARMADILLO=OFF"
                          "-DMAPHYS_USE_EIGEN=OFF"
                          "-DMAPHYS_USE_FABULOUS=OFF"
                          "-DMAPHYS_DEV_TANGLE=OFF"
                          "-DMAPHYS_USE_PASTIX=ON"
                          "-DMAPHYS_USE_MUMPS=ON")
                        #:phases (modify-phases %standard-phases
                                                (add-before 'check 'prepare-test-environment
                                                            (lambda _
                                                              ;; Allow tests with more MPI processes than available CPU cores,
                                                              ;; which is not allowed by default by OpenMPI
                                                              (setenv "OMPI_MCA_rmaps_base_oversubscribe" "1") #t)))
                        ))
   (build-system cmake-build-system)
   (inputs `(("blaspp" ,blaspp)
             ("lapackpp" ,lapackpp)
             ("pastix" ,pastix)
             ("mumps" ,mumps-openmpi)
             ("scalapack" ,scalapack) ;; should be inherited from mumps?
             ))
   (propagated-inputs `(("mpi" ,openmpi)
                        ("ssh" ,openssh)))
   (native-inputs `(("gfortran" ,gfortran)
                    ("pkg-config" ,pkg-config)))
   ))

(define-public blaspp
  (package
    (name "blaspp")
    (version "2020.08.00")
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
                   (commit "049c515c1fafee62b4c1d2870621d6819698f71a")))
             (file-name (string-append name "-" version "-checkout"))
             ;;(patches (search-patches "inria/patches/blaspp-installation-directories.patch"))
             (sha256
              (base32
               "1p3a7sgmy5m2j28m1qnj70fzzmqn2qdrk1bsv4yzpk2a55hfrlqb"))))
    (arguments
     '(#:configure-flags '("-DBLASPP_BUILD_TESTS=OFF")
                         #:tests? #f))
    ;; tests would need testsweeper https://bitbucket.org/icl/testsweeper
    ;;'(#:configure-flags '("-DBLASPP_BUILD_TESTS=ON")))
    (build-system cmake-build-system)
    (propagated-inputs `(("openblas" ,openblas))) ;; technically only blas
    (license #f)))

(define-public lapackpp
  (package
   (name "lapackpp")
   (version "0.1")
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
                  (commit "e3052eb11b9bab7a7adfe246a53abe8a31c52b3b")))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32
              "039lyh829xvjc7jgmv39bgy87ap94pqffh52457d0x16mp64qbhn"))))
   (arguments
    '(#:configure-flags '("-DBUILD_LAPACKPP_TESTS=OFF"
                          "-DBUILD_SHARED_LIBS=ON")
                        #:tests? #f))
    ;; tests would need testsweeper https://bitbucket.org/icl/testsweeper
    (build-system cmake-build-system)
    (inputs `(("blaspp" ,blaspp)))
    (license #f)))

(define-public pastix-6
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
                           "-DPASTIX_WITH_STARPU=ON"
                           "-DPASTIX_ORDERING_METIS=OFF"
                           "-DPASTIX_ORDERING_SCOTCH=ON")

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
                                               (getenv "PYTHONPATH")))
                        #t))))

       ;; XXX: The 'python_simple' test fails with:
       ;;   ValueError: Attempted relative import in non-package
       #:tests? #f))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gfortran" ,gfortran)))
    (inputs
     `(("gfortran:lib" ,gfortran "lib")           ;for 'gcc … -lgfortran'
       ("openblas" ,openblas)
       ;; ("lapack" ,lapack)         ;must be built with '-DLAPACKE_WITH_TMG=ON'

       ;; The following are optional dependencies.
       ("starpu" ,starpu)

       ;; Python bindings and Python tests. Python3
       ("python" ,python)

       ("python-numpy" ,python-numpy)
       ;;("python-scipy" ,python-scipy)
       ))
    (propagated-inputs `(("hwloc" ,hwloc "lib")
                         ("scotch" ,scotch)))
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
CF90CCPOPT  = -ffree-form -x f95-cpp-input
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
   `(("gfortran:lib" ,gfortran "lib")
     ("openblas" ,openblas)))
  (native-inputs
   `(("pkg-config" ,pkg-config)
     ("gfortran" ,gfortran)
     ("perl" ,perl)))
  (propagated-inputs
   `(("openmpi" ,openmpi-with-mpi1-compat)
     ("hwloc" ,hwloc-1 "lib")
     ("openssh" ,openssh)
     ("scotch32" ,scotch32)))
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
                                   `(cons "-DPASTIX_BUILD_TESTING=OFF" ,flags))))
   (inputs `(
             ,@(delete `("python2" ,python-2) (package-inputs pastix))
             ,@(delete `("python-numpy" ,python2-numpy) (package-inputs pastix))
             ;; ,@(delete `("python-scipy" ,python2-scipy) (package-inputs pastix))
             ))))

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

    (inputs `(("recutils" ,recutils)))
    (native-inputs `(("gcc-toolchain" ,gcc-toolchain)))))

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
   (propagated-inputs `(("openmpi" ,openmpi)))
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
