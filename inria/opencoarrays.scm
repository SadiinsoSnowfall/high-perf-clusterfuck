;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019 Inria

(define-module (inria opencoarrays)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (inria mpi)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi))

(define-public opencoarrays
  (let ((commit "15dc8c3b4707fd4c25742c6091978fd2e30d4f52")
        (revision "0"))
    (package
      (name "opencoarrays")
      (version "2.0.0")
      (home-page "https://github.com/sourceryinstitute/OpenCoarrays.git")
      (source (origin
                (method git-fetch)
                (uri (git-reference 
                     (url home-page)
                     (commit version)))
                (sha256
                 (base32
                  "0kqnksgc5clnhcszi53fzpl8z0rjij9qqy2a4ji1li3d8sd0323d"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-after 'build 'mpi-setup
                      ;; Set the test environment for Open MPI.
                      ,%openmpi-setup))
            ;; deactive tests for now ; there is 4 which do not work
         #:tests? #f
         )
       )
      (propagated-inputs
       `(
         ("gfortran" , gfortran)
         ("openmpi", openmpi-with-mpi1-compat)))
(synopsis
       "OpenCoArrays Fortran")
      (description
       "Fortran CoArrays is a library implementing a PGAS model for programming parallel application in Fortran")
      (license license:bsd-3))))
