;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2022 Inria

(define-module (inria freefem)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module ((guix licenses) #:prefix license:))

(define-public freefem
  (package
    (name "freefem")
    (version "4.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FreeFem/FreeFem-sources")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qsx3jvipnrsd6x7m38mnj6dixxsf70ar80b9gy4rnjrsbdf6iqh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'check 'mpi-setup
                    ;; Set the test environment for Open MPI.
                    ,%openmpi-setup))))
    (native-inputs
     (list autoconf
           automake
           unzip
           bison
           flex
           gfortran))
    (inputs
     ;; FIXME: Tests under 'examples/eigen', built when ARPACK is available,
     ;; fail.
     (list ;; ("arpack-ng" ,arpack-ng)
           ;; petsc-openmpi
           gsl
           ipopt
           nlopt
           openmpi
           lapack))
    (properties `((tunable? . #true)))
    (home-page "https://freefem.org/")
    (synopsis "High-level multiphysics finite element library")
    (description
     "FreeFEM is a partial differential equation solver for non-linear
multi-physics systems in 2D and 3D using the finite element method.

Problems involving partial differential equations from several branches of
physics such as fluid-structure interactions require interpolations of data
on several meshes and their manipulation within one program.

FreeFEM includes a fast interpolation algorithm and a language for the
manipulation of data on multiple meshes. It is written in C++ and the FreeFEM
language is a C++ idiom.")
    (license license:lgpl3+)))
