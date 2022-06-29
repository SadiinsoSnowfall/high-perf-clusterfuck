;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019, 2020 Inria

(define-module (inria memphis)
  #:use-module (guix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages xml)
  )

(define-public bitpit
  (package
   (name "bitpit")
   (version "1.7.1")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://github.com/optimad/bitpit/archive/refs/tags/bitpit-1.7.1.tar.gz")
     (sha256
      (base32
       "04438fd94a6px6al0cqcfas5qcaqbpsiic8h2vzn3jgbcm384vr0"))))
   (inputs
    (list libxml2 openblas))
   (propagated-inputs
    (list openmpi petsc-openmpi))
   (native-inputs
    (list pkg-config gfortran openssh))
   (build-system cmake-build-system)
   (arguments
    '(#:configure-flags `("-DCMAKE_NO_SYSTEM_FROM_IMPORTED=ON"
                          "-DBUILD_SHARED_LIBS=ON"
                          "-DENABLE_MPI=ON"
                          ,(string-append "-DPETSC_DIR="
                                          (assoc-ref %build-inputs "petsc-openmpi")))
      #:tests? #f))
   (home-page "http://optimad.github.io/bitpit/")
   (synopsis
    "C++ parallel library for structured and unstructured mesh handling.")
   (description
    "C++ parallel library for structured and unstructured mesh handling.")
   (license license:lgpl3)))
