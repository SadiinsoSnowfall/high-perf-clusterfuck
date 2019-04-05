;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019 Inria

(define-module (inria pampa)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (inria mpi)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi))

(define-public pampa
  (let ((commit "2d2d89ec91c514dbf5a63cf6b746e168eece36fe")
        (revision "0"))
    (package
      (name "pampa")
      (version (git-version "0.0" revision commit))
      (home-page "https://gitlab.inria.fr/PaMPA/PaMPA")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page)
                                    (commit commit)))
                (sha256
                 (base32
                  "0rpvv5bc7x8jbwjnx06rnfvdrwppnlx1hhsn2qf5jvcqhi3hryga"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-after 'build 'mpi-setup
                      ;; Set the test environment for Open MPI.
                      ,%openmpi-setup))))
      (native-inputs
       `(("gfortran" ,gfortran)))
      (inputs
       `(("zlib" ,zlib)
         ("pt-scotch" ,pt-scotch32)

         ;; PaMPA relies on deprecated MPI1 functionality such as
         ;; 'MPI_Type_extent'.
         ("openmpi" ,openmpi-with-mpi1-compat)))
      (synopsis
       "Dynamic parallel remeshing and redistribution of unstructured meshes")
      (description
       "PaMPA is a C library for dynamic parallel remeshing and
redistribution of unstructured meshes.")
      (license license:gpl3+))))
