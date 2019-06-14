;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019 Inria

(define-module (inria vite)
  #:use-module (guix packages)
  #:use-module (guix svn-download)
  #:use-module (guix licenses)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages gl))

(define-public vite
  (let ((revision 1610))
    (package
      (name "vite")
      (version (string-append "1.2." (number->string revision)))
      (source (origin
                (uri (svn-reference
                      (url "https://scm.gforge.inria.fr/anonscm/svn/vite/trunk")
                      (revision revision)))
                (method svn-fetch)
                (file-name (string-append "vite-" version "-checkout"))
                (sha256
                 (base32
                  "1icvscxyx6cbc7agj85dydlcx97d3zsi8m02ha46v3kmxlsm4qy9"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags (list "-DUSE_QT5=TRUE"

                                 ;; XXX: This requires a very old version of
                                 ;; Boost, older than 1.59.
                                 ;; "-DVITE_ENABLE_SERIALIZATION=TRUE"

                                 ;; TODO: Package OTF.
                                 ;; "-DVITE_ENABLE_OTF=TRUE"
                                 )
         #:tests? #f))
      (inputs
       `(("qtbase" ,qtbase)
         ("mesa" ,mesa)
         ("glu" ,glu)))
      (native-inputs
       `(("qttools" ,qttools)))
      (home-page "http://vite.gforge.inria.fr/")
      (synopsis "Visualize program execution traces")
      (description
       "ViTE is a trace explorer.  It is a tool to visualize execution traces
of parallel programs (OpenMP, MPI, etc.) in Pajé or OTF format for debugging
and profiling parallel or distributed applications.  Such traces can be
obtained using, for example, EZTrace.")
      (license cecill))))
