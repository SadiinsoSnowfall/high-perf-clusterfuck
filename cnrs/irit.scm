;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Note that this module provides packages that depend on "non-free"
;;; software, which denies users the ability to study and modify it.
;;;
;;; Copyright © 2019 Inria

(define-module (cnrs irit)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix git)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages wget)
  #:use-module (guix utils)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (inria hiepacs)
  #:use-module (inria storm)
  #:use-module (inria tadaam))

(define-public qr_mumps
  (package
   (name "qr_mumps")
   (version "3.0.3")
   (home-page "http://buttari.perso.enseeiht.fr/qr_mumps/")
   (source (origin
            (method url-fetch)
            (uri
             (string-append home-page "releases/qr_mumps-" version ".tgz"))
            (sha256
             (base32
              "16pc843p2zx29zxvnr6hgq674xfkqkvxyy7cx428rc2v2bkg70xx"))))
   (build-system cmake-build-system)
   (arguments
    '(#:configure-flags  (list
                          "-Wno-dev"
                          "-DBUILD_SHARED_LIBS=ON"
                          (string-append "-DCMAKE_EXE_LINKER_FLAGS="
                                         "-Wl,-rpath="
                                         (assoc-ref %outputs "out")
                                         "/lib")
                          "-DQRM_WITH_STARPU=ON"
                          "-DQRM_ORDERING_SCOTCH=ON"
                          "-DQRM_ORDERING_METIS=ON"
			                    "-DQRM_WITH_MPI=ON"
                          )
      #:phases (modify-phases %standard-phases
                              (add-before 'check 'prepare-test-environment
			                                    (lambda _
				                                    (setenv "HOME" (getcwd)) ;; StarPU expects $HOME to be writable.
	                                          (setenv "OMP_NUM_THREADS" "1")
				                                    (setenv "OMPI_MCA_rmaps_base_oversubscribe" "1") #t)))
      #:tests? #f))

   (native-inputs `(("gfortran" ,gfortran)
                    ("pkg-config" ,pkg-config)))
   (inputs `(("metis" ,metis)
             ("openblas" ,openblas)
             ("perl" ,perl)
             ("scotch32" ,scotch32)
	           ("ssh" ,openssh)
             ;; ("suitesparse" ,suitesparse) ;; for colamd; it would ideally be suitesparse:colamdonly
             ("wget" ,wget)
             ))
   (propagated-inputs `(("starpu" ,starpu)))
   (synopsis "Sparse QR direct solver (experimental package for distributed memroy version)")
   (description
    "qr_mumps is a software package for the solution of sparse, linear systems
on multicore computers based on the QR factorization of the input matrix.
Therefore, it is suited to solving sparse least-squares problems and to
computing the minimum-norm solution of sparse, underdetermined problems. It can
obviously be used for solving square problems in which case the stability
provided by the use of orthogonal transformations comes at the cost of a higher
operation count with respect to solvers based on, e.g., the LU factorization.
qr_mumps supports real and complex, single or double precision arithmetic. This
is an experimental version of the package for distributed memory." )
   (license license:cecill)))
