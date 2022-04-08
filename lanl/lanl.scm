;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Note that this module provides packages that depend on "non-free"
;;; software, which denies users the ability to study and modify it.
;;;
;;; Copyright © 2022 Inria

(define-module (lanl lanl)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix packages) ; for guix style
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git)
  #:use-module (gnu packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages perl)
  #:use-module (guix utils)
  #:use-module (gnu packages mpi)
  )

(define-public ior
  (package
    (name "ior")
    (version "3.4.0") ; not official release, but visible in program output
    (home-page "https://github.com/hpc/ior")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "8475c7d30025dd5e39147c251bf84e1ed24b9858")))
              (file-name (string-append name "-checkout"))
              (sha256
               (base32
                "0brr1ng8qdivhlcbppz3mf7ml88hffj201lw08i0gkzikay3d7i3"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'check)))) ;TODO: make check is buggy in Guix
    (native-inputs (list autoconf automake perl))
    (inputs (list openmpi))
    (synopsis "Parallel IO benchmark tool")
    (description
     "IOR is a parallel IO benchmark that can be used to test the
        performance of parallel storage systems using various interfaces and
        access patterns.  The IOR repository also includes the mdtest benchmark
        which specifically tests the peak metadata rates of storage systems
        under different directory structures.  Both benchmarks use a common
        parallel I/O abstraction backend and rely on MPI for synchronization.")
    (license license:gpl2)))
