;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019, 2021, 2022 Inria

(define-module (inria bioversiton)
  #:use-module (guix)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  )

(define-public disseq
  (package
   (name "disseq")
   (version "0")
   (home-page "https://gitlab.inria.fr/biodiversiton/disseq")
   (source (git-checkout (url "https://gitlab.inria.fr/biodiversiton/disseq.git")
                         (branch "master")))
   (build-system cmake-build-system)
   (arguments
    '(#:configure-flags `("-DBUILD_SHARED_LIBS=OFF"
                          "-DCMAKE_NO_SYSTEM_FROM_IMPORTED=ON")
                        #:phases (modify-phases %standard-phases
                                                (add-after 'unpack 'chdir
                                                           (lambda _
                                                             (chdir "src"))))
                        #:tests? #f))

   (inputs (list openmpi hdf5-parallel-openmpi))

   (synopsis "Compute pairwise distances between reads as edit distances")
   (description
    "This package has been developed for computing exact distances,
     without heuristics, between all pairs of reads of a NGS
     sample. This is a first step for supervised or unsupervised clustering
     of reads in an environmental sample.")
   (license license:gpl3)))
