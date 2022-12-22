;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019, 2021 Inria

(define-module (inria vite)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages maths))

(define S specification->package)

(define-public vite
  (let ((commit "442cf13b6f8025eba09f0b4165e9a789d40409ae")
        (revision "1"))
    (package
      (name "vite")
      (version (git-version "1.2" revision commit))
      (home-page "https://gitlab.inria.fr/solverstack/vite/")
      (source (origin
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (method git-fetch)
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09ymsab03qvbxn6zbzh660aa4746b4sn0v8644y7k54pjhk76372"))))
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
       (list (S "qtbase@5")
             mesa
             glew
             glu
             glm
             (S "qtcharts@5")
             (S "qtsvg@5")))
      (native-inputs
       (list (S "qttools@5")))
      (synopsis "Visualize program execution traces")
      (description
       "ViTE is a trace explorer.  It is a tool to visualize execution traces
of parallel programs (OpenMP, MPI, etc.) in Pajé or OTF format for debugging
and profiling parallel or distributed applications.  Such traces can be
obtained using, for example, EZTrace.")
      (license cecill))))
