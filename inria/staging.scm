;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019 Inria

(define-module (inria staging)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg))

;;; Commentary:
;;;
;;; This is the staging area: things that ought to be in Guix proper but more
;;; work is needed before that can happen.
;;;
;;; Code:

(define-public paraview
  ;; TODO: This should be part of Guix proper, but first, we should try
  ;; unbundling VTK.
  (package
    (name "paraview")
    (version "5.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Kitware/paraview.git")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0v67fvjdv4x5p69qdknnsbsqvp1znvbi8pmmzrz50dp33p1gd44z"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DPARAVIEW_ENABLE_PYTHON=ON"
                           ;; "-DPARAVIEW_USE_EXTERNAL=ON"
                           "-DPARAVIEW_USE_EXTERNAL_VTK=OFF" ;XXX
                           ;; "-DPARAVIEW_ENABLE_WEB=OFF"
                           ;; "-DPARAVIEW_ENABLE_EMBEDDED_DOCUMENTATION=OFF"
                           ;; "-DOpenGL_GL_PREFERENCE=GLVND"
                           )))
    ;; FIXME: "include/paraview-5.7/vtkConfigure.h" defines
    ;; 'VTK_CXX_COMPILER' as the absolute file name of 'c++'.  Remove that so
    ;; we don't keep a reference to GCC.
    (native-inputs
     `(("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("qtxmlpatterns" ,qtxmlpatterns)
       ("libx11" ,libx11)
       ("libxt" ,libxt)
       ("mesa" ,mesa)
       ("glu" ,glu)
       ("python" ,python)
       ("python-numpy" ,python-numpy)
       ;; ("utf8cpp" ,utf8cpp)
       ("vtk" ,vtk)
       ("hdf5" ,hdf5)
       ("protobuf" ,protobuf)))
    (synopsis "Data analysis and visualization application")
    (description
     "ParaView is a data analysis and visualization application.  It allows
users to quickly build visualizations to analyze their data using qualitative
and quantitative techniques.  The data exploration can be done interactively
in 3D or programmatically using ParaView's batch processing capabilities.

ParaView was developed to analyze extremely large datasets using distributed
memory computing resources.  It can be run on supercomputers to analyze
datasets of petascale size as well as on laptops for smaller data.")
    (home-page "https://www.paraview.org/")
    (license license:bsd-3)))
