;;; This module extends GNU�| Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019 Inria
;;; Copyright © 2020, 2022 Michele MARTONE (LRZ)

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
  #:use-module (gnu packages base)
  )

(define-public librsb
  (package
    (name "librsb")
    (version "1.3")
    (source (origin
              (uri "http://download.sourceforge.net/librsb/librsb-1.3.0.0.tar.gz")
              (method url-fetch)
              (sha256
               (base32
                "1457d3l5xa9fnvmk2z8q6p4iw8pdn85npbl3jggmg3wq3xfp5j1a"))
              ))
    (build-system gnu-build-system)
    (inputs `(("m4", m4)
	      ("libtool", libtool)
	      ("autoconf", autoconf)
	      ("automake", automake)
        ("which", which)))
    (synopsis "librsb: A shared memory parallel sparse matrix computations library for the Recursive Sparse Blocks format")
    (description
     "librsb is a library for sparse matrix computations featuring the Recursive Sparse Blocks (RSB) matrix format. This format allows cache efficient and multi-threaded (that is, shared memory parallel) operations on large sparse matrices. The most common operations necessary to iterative solvers are available, e.g.: matrix-vector multiplication, triangular solution, rows/columns scaling, diagonal extraction / setting, blocks extraction, norm computation, formats conversion. The RSB format is especially well suited for symmetric and transposed multiplication variants. Most numerical kernels code is auto generated, and the supported numerical types can be chosen by the user at build time. librsb can also be built serially (without OpenMP parallelism), if required.
librsb also implements the Sparse BLAS standard, as specified in the [BLAS Technical Forum] documents.
")
    (license license:lgpl3+)
    (home-page "http://librsb.sourceforge.net/")))
