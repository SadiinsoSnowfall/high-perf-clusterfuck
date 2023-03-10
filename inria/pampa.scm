;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019, 2020 Inria

(define-module (inria pampa)
  #:use-module (guix)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (inria mpi)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi))

(define openmpi-with-mpi1-compat-instead-of-openmpi
  ;; This is a procedure to replace OPENMPI by OPENMPI-WITH-MPI1-COMPAT,
  ;; recursively.
  (package-input-rewriting `((,openmpi . ,openmpi-with-mpi1-compat))))

(define pt-scotch32-with-mpi1-compat
  (openmpi-with-mpi1-compat-instead-of-openmpi pt-scotch32))

(define-public pampa
  (let ((commit "db630b6532666ab2f3c970b161ec2cfdfc074bae")
        (revision "1"))
    (package
      (name "pampa")
      (version (git-version "0.0" revision commit))
      (home-page "https://gitlab.inria.fr/PaMPA/PaMPA")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "1vlvyi2rnj9k049l5k659avk06fls20yw2blxxbvfzw0v1pn9z1z"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
         #:phases (modify-phases %standard-phases
                    (add-after 'build 'mpi-setup
                      ;; Set the test environment for Open MPI.
                      ,%openmpi-setup))
         ;; FIXME: We get several test failures most likely due to numerical
         ;; instability:
         ;;
         ;; The following tests FAILED:
         ;; 152 - overlap_1_11-vert_3-proc_it_c (Failed)
         ;; 259 - MEM_redist_11-vert_12-proc_it_c (Failed)
         ;; 271 - MEM_redist_11-vert_12-proc_it_comm_c (Failed)
         ;; 283 - MEM_redist_11-vert_12-proc_it_comm2_c (Failed)
         ;; 295 - MEM_redist_11-vert_12-proc_it_comm3_c (Failed)
         ;; 307 - MEM_redist2_11-vert_12-proc_it_c (Failed)
         ;; 319 - MEM_redist2_11-vert_12-proc_it_comm_c (Failed)
         ;; 331 - MEM_redist2_11-vert_12-proc_it_comm2_c (Failed)
         ;; 343 - MEM_redist2_11-vert_12-proc_it_comm3_c (Failed)
         ;; Errors while running CTest
         #:tests? #f))
      (native-inputs (list gfortran))
      ;; PaMPA relies on deprecated MPI1 functionality such as
      ;; 'MPI_Type_extent'.
      (inputs (list zlib pt-scotch32-with-mpi1-compat openmpi-with-mpi1-compat))
      (synopsis
       "Dynamic parallel remeshing and redistribution of unstructured meshes")
      (description "PaMPA is a C library for dynamic parallel remeshing and
redistribution of unstructured meshes.")
      (license license:gpl3+))))
