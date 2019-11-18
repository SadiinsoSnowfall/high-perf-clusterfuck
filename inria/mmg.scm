;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019 Inria

(define-module (inria mmg)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix licenses))

(define-public mmg
  (package
    (name "mmg")
    (version "5.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MmgTools/mmg.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1bck0pwpzd0srx03v7hk9ij91s3r3k9mc38xpw78l51q6x1nvzmd"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_TESTING=ON"
                           "-DONLY_VERY_SHORT_TESTS=ON")))
    (home-page "https://www.mmgtools.org/")
    (synopsis "Library and applications for simplicial mesh adaptation")
    (description
     "Mmg gather open-source software for simplicial mesh
modifications (2D, 3D surfacic and 3D volumic).  It allows for: mesh quality
improvement, mesh adaptation on an isotropic or anisotropic sizemap, isovalue
discretization, and Lagrangian movement.")
    (license lgpl3+)))
