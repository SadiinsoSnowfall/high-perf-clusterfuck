;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017 Inria

(define-module (inria storm)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix licenses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages gcc)
  #:use-module (inria simgrid)
  #:use-module (ice-9 match))

(define %starpu-home-page
  "http://starpu.gforge.inria.fr")

(define (starpu-url version)
  (string-append %starpu-home-page "/files/starpu-"
                 version "/starpu-" version ".tar.gz"))

(define-public starpu-1.1
  (package
    (name "starpu")
    (version "1.1.7")
    (home-page %starpu-home-page)
    (source (origin
              (method url-fetch)
              (uri (starpu-url version))
              (sha256
               (base32
                "09nbns7wq4nhw8771jfd6rdz0zxdrh2c3fvanv4hvinnrhwp5xlr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'set-home
                    (lambda _
                      ;; Some of the tests under tools/ expect $HOME to be
                      ;; writable.
                      (setenv "HOME" (getcwd))
                      #t)))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("fftw" ,fftw)
              ("fftwf" ,fftwf)
              ("hwloc" ,hwloc)))
    (synopsis "Run-time system for heterogeneous computing")
    (description
     "StarPU is a run-time system that offers support for heterogeneous
multicore machines.  While many efforts are devoted to design efficient
computation kernels for those architectures (e.g. to implement BLAS kernels
on GPUs), StarPU not only takes care of offloading such kernels (and
implementing data coherency across the machine), but it also makes sure the
kernels are executed as efficiently as possible.")
    (license lgpl2.1+)))


(define-public starpu
  (package
    (inherit starpu-1.1)
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (starpu-url version))
              (sha256
               (base32
                "0xwd9yh0fr7yv122x0kayrdw586pffkhqd3krv1g7xvvpvwi631f"))))))

(define-public starpu+simgrid
  (package
    (inherit starpu)
    (name "starpu-simgrid")
    (inputs `(("simgrid" ,simgrid)
              ,@(package-inputs starpu)))
    (arguments `(#:configure-flags '("--enable-simgrid")))))

;; TODO: Add variants with MPI support.
