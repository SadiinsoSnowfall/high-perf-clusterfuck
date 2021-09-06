;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019, 2021 Inria

(define-module (airbus solvers)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages mpi)
  #:use-module (inria storm)
  #:use-module (inria hiepacs)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public hmat-oss
  (let
      ((commit "6a4e21bb86cda66396f27d3473471c52a2357edc")
       (revision "32"))
    (package
     (name "hmat-oss")
     (version
      (git-version "1.7.1" revision commit))
     (home-page "https://github.com/jeromerobert/hmat-oss")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (sha256
        (base32
         "1alkzahpw55bqy01yscvl7sl2r4mhyv7i1qs12wmzhapqsw36jvg"))))
     (build-system cmake-build-system)
     (arguments
      '(#:configure-flags '("-DBUILD_EXAMPLES=ON")))
     (inputs
      `(("jemalloc" ,jemalloc)
        ("lapack" ,openblas)))
     (synopsis "A hierarchical matrix C/C++ library including an LU solver.")
     (description #f)
     (license license:gpl2))))

(define-public test_FEMBEM
  (let
      ((commit "59d1983493a95ec483b4d931afcc70b687474aa5")
       (revision "64"))
    (package
     (name "test_FEMBEM")
     (version
      (git-version "0.1" revision commit))
     (home-page "https://gitlab.inria.fr/solverstack/test_fembem")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (sha256
        (base32
         "0ahwzappkfbazni5sf3313diipjb927z3kjmxsykczdpzvk8bny6"))))
     (build-system cmake-build-system)
     (arguments
      ;; The package checkout is not a Git directory anymore even if its
      ;; original source is. For the configure phase to work, we need to prevent
      ;; CMake from trying to detect the version number using Git.
      '(#:configure-flags '("-DTEST_FEMBEM_GIT_VERSION=OFF")
        #:phases
        (modify-phases %standard-phases
                       (add-before 'check 'prepare-test-environment
                                   (lambda _
                                     ;; StarPU requires the $HOME folder to be
                                     ;; writable during the test phase. Given
                                     ;; that the original home directory is not
                                     ;; writable during package construction, we
                                     ;; set HOME to the current build directory
                                     ;; to satisfy StarPU.
                                     (setenv "HOME" (getcwd)) #t)))))
     (native-inputs
      `(("pkg-config" ,pkg-config)))
     (inputs
      `(("mpi" ,openmpi)
        ("openmp" ,libomp)
        ("lapack" ,openblas)
        ("openssh" ,openssh)
        ("starpu" ,starpu)
        ("chameleon" ,chameleon)
        ("hmat-oss" ,hmat-oss)))
     (synopsis
      "A simple application for testing dense and sparse solvers with pseudo-FEM
or pseudo-BEM matrices.")
     (description
      "This is an open-source version of a simple application developed by
Airbus for testing dense and sparse solvers with pseudo-FEM or pseudo-BEM
matrices. Here in Guix, test_FEMBEM is currently connected to HMAT-OSS,
Chameleon and H-Chameleon.")
     (license license:expat))))
