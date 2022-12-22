(define-module (teaching nachos)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix memoization)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages perl)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (lrz librsb)
  )

(define-public nachos
  (package
    (name "nachos")
    (version "4.0")
    (home-page "https://gitlab.inria.fr/agullo-teach/ub/m1/nachos")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url home-page)
                   (commit "531288c95467d3a39fb75dd50a47f0fe3a0dc321")))
             (file-name (string-append name "-" version "-checkout"))
             (sha256
              (base32
               "1p1bibbcc8yzy52zpv9r2sx02235yh0ssfx0nm0pvplkblfrlh02"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no test suite
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No ./configure script
         )))
    (synopsis "Not Another Completely Heuristic Operating System (Nachos) - Kernel part only")
    (description "Not Another Completely Heuristic Operating System, or Nachos, is instructional
software for teaching undergraduate, and potentially graduate level operating
systems courses. As this stage, the package only processes the kernel part (but
not the ./test/ part).")
    (license license:bsd-3)
    (inputs (list grep sed bash))))


