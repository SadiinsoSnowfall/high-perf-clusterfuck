;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019 Inria

(define-module (inria mmg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix licenses))

(define-public mmg
  (package
    (name "mmg")
    (version "5.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/MmgTools/mmg/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32
         "0fpr0y1fy9wyda4678qzjlzravh3cns67m3b79b31bqqqjk851zb"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_TESTING=ON"
                           "-DONLY_VERY_SHORT_TESTS=ON")
       #:tests? #t))
    (home-page "https://www.mmgtools.org/")
    (synopsis "Library and applications for simplicial mesh adaptation.")
    (description
     "Mmg gather open-source software for simplicial mesh modifications
     (2D, 3D surfacic and 3D volumic). It allows: mesh quality improvement,
      mesh adaptation on an isotropic or anisotropic sizemap,
     isovalue discretization and lagrangian movement.")
    (license lgpl3+)
    )
  )
