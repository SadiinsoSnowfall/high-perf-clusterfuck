;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019 Inria

(define-module (inria mpi)
  #:use-module (guix)
  #:use-module (guix utils)
  #:use-module (gnu packages mpi)
  #:use-module (srfi srfi-1))

(define-public openmpi-with-mpi1-compat
  ;; In Open MPI 4 the deprecated MPI1 functions are disabled by default.
  ;; This variant enables them.
  (package
    (inherit openmpi)
    (name "openmpi-mpi1-compat")
    (arguments
     (substitute-keyword-arguments (package-arguments openmpi)
       ((#:configure-flags flags ''())
        `(cons "--enable-mpi1-compatibility" ,flags))))

    ;; Depend on hwloc 1.x because that's what users of this package expect.
    (inputs `(("hwloc" ,hwloc-1 "lib")
              ,@(alist-delete "hwloc" (package-inputs openmpi))))))
