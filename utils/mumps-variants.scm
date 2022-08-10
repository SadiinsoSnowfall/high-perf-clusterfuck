;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2022 Inria

(define-module (utils mumps-variants)
  #:use-module (guix packages)
  #:use-module (gnu packages maths)
  #:use-module (srfi srfi-1))

(define-public mumps-scotch32-openmpi
  (package
   (inherit mumps-openmpi)
   (name "mumps-scotch32-openmpi")
   (inputs
    `(("scotch" ,scotch32)
      ,@(alist-delete "pt-scotch" (package-inputs mumps-openmpi))))))
