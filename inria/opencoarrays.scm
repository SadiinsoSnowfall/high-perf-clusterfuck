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
  #:use-module (gnu packages perl)
  #:use-module (gnu packages mpi))

(define-public opencoarrays
  (let ((commit "15dc8c3b4707fd4c25742c6091978fd2e30d4f52")
        (revision "0"))
    (package
      (name "opencoarrays")
      (version "2.9.2")
      (home-page "https://github.com/sourceryinstitute/OpenCoarrays.git")
      (source (origin
                (method git-fetch)
                (uri (git-reference 
                     (url home-page)
                     (commit version)))
                (sha256
                 (base32
                  "1zp8jdw1dks289gjfbm7f7l77fsb0acshq9l3c37s10ppka4p5w9"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-after 'build 'mpi-setup
                      ;; Set the test environment for Open MPI.
                      ,%openmpi-setup))
            ;; deactive tests for now ; there is 4 which do not work
         #:tests? #t
         )
       )
      (propagated-inputs
       `(
         ("gfortran" , gfortran)
         ("perl" , perl)
         ("openmpi", openmpi)))
(synopsis
       "OpenCoArrays Fortran")
      (description
       "Fortran CoArrays is a library implementing a PGAS model for programming parallel application in Fortran")
      (license license:bsd-3))))
