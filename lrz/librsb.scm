;;; This module extends GNU�| Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019 Inria
;;; Copyright © 2020 Michele MARTONE (LRZ)

;;; This module has been developed under the PRACE-6IP-WP8 EU project LyNcs (GRANT AGREEMENT NUMBER 823767 -- PRACE-6IP).

(define-module (lrz librsb)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages mpi) ;; for hwloc
  #:use-module (gnu packages m4)
  #:use-module (gnu packages autotools)
  )

(define-public librsb
  (package
    (name "librsb")
    (version "1.2.0.9")
    (source (origin
              (uri "http://download.sourceforge.net/librsb/librsb-1.2.0.9.tar.gz")
              (method url-fetch)
              (sha256
               (base32
                "1ynrsgnvv1jfm8dv3jwjrip9x9icxv7w3qrk149025j6fbaza8gl"))
              ))
    (build-system gnu-build-system)
    (inputs `(("m4", m4)
	      ("libtool", libtool)
	      ("autoconf", autoconf)
	      ("automake", automake)))
    (synopsis "librsb: A shared memory parallel sparse matrix computations library for the Recursive Sparse Blocks format")
    (description
     "librsb is a library for sparse matrix computations featuring the Recursive Sparse Blocks (RSB) matrix format. This format allows cache efficient and multi-threaded (that is, shared memory parallel) operations on large sparse matrices. The most common operations necessary to iterative solvers are available, e.g.: matrix-vector multiplication, triangular solution, rows/columns scaling, diagonal extraction / setting, blocks extraction, norm computation, formats conversion. The RSB format is especially well suited for symmetric and transposed multiplication variants. Most numerical kernels code is auto generated, and the supported numerical types can be chosen by the user at build time. librsb can also be built serially (without OpenMP parallelism), if required.
librsb also implements the Sparse BLAS standard, as specified in the [BLAS Technical Forum] documents.
")
    (license license:lgpl3+)
    (home-page "http://librsb.sourceforge.net/")))
