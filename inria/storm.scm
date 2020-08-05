;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2018, 2019 Inria

(define-module (inria storm)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages python)
  #:use-module (inria tadaam)
  #:use-module (inria eztrace)
  #:use-module (inria mpi)
  #:use-module (inria simgrid)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define %starpu-home-page
  "http://starpu.gforge.inria.fr")

(define %patch-path "inria/patches/starpu_skip_apps.patch")

(define (starpu-url version)
  (string-append %starpu-home-page "/files/starpu-"
                 version "/starpu-" version ".tar.gz"))

(define %starpu-git "https://gitlab.inria.fr/starpu/starpu.git")

(define-public starpu-1.1
  (package
    (name "starpu")
    (version "1.1.7")
    (home-page %starpu-home-page)
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url %starpu-git)
                   (commit (string-append "starpu-" version))))
             (file-name (string-append name "-" version "-checkout"))
             (sha256
              (base32 "18m99yl03ps3drmd6vchrz6xchw8hs5syzi1nsy0mniy1gyz7glw"))))
    (build-system gnu-build-system)
    (outputs '("debug" "out"))
    (arguments
     '(#:configure-flags '("--enable-quick-check") ;make tests less expensive

       ;; Various files are created in parallel and non-atomically, notably
       ;; in ~/.cache/starpu.
       #:parallel-tests? #f

       #:phases (modify-phases %standard-phases
                  (add-before 'check 'pre-check
                    (lambda _
                      ;; Some of the tests under tools/ expect $HOME to be
                      ;; writable.
                      (setenv "HOME" (getcwd))

                      ;; Increase the timeout for individual tests in case
                      ;; we're building on a slow machine.
                      (setenv "STARPU_TIMEOUT_ENV" "1200")

                      ;; Allow us to dump core during the test suite so GDB
                      ;; can report backtraces.
                      (let ((MiB (* 1024 1024)))
                        (setrlimit 'core (* 300 MiB) (* 500 MiB)))
                      #t))
                  (replace 'check
                    (lambda args
                      ;; To ease debugging, display the build log upon
                      ;; failure.
                      (let ((check (assoc-ref %standard-phases 'check)))
                        (or (apply check #:make-flags '("-k") args)
                            (begin
                              (display "\n\nTest suite failed, dumping logs.\n\n"
                                       (current-error-port))
                              (for-each (lambda (file)
                                          (format #t "--- ~a~%" file)
                                          (call-with-input-file file
                                            (lambda (port)
                                              (dump-port port
                                                         (current-error-port)))))
                                        (find-files "." "^test-suite\\.log$"))
                              #f))))))))
    (native-inputs
     `(("gfortran" ,gfortran)
       ("pkg-config" ,pkg-config)
       ("gdb" ,gdb)
       ("libtool" ,libtool)
       ("autoconf" ,autoconf)
       ("automake" ,automake)))                             ;used upon test failure
    (inputs `(("fftw" ,fftw)
              ("fftwf" ,fftwf)))
    (propagated-inputs  `(("hwloc" ,hwloc-1 "lib")
                          ("mpi" ,openmpi-with-mpi1-compat)))
    (synopsis "Run-time system for heterogeneous computing")
    (description
     "StarPU is a run-time system that offers support for heterogeneous
multicore machines.  While many efforts are devoted to design efficient
computation kernels for those architectures (e.g. to implement BLAS kernels
on GPUs), StarPU not only takes care of offloading such kernels (and
implementing data coherency across the machine), but it also makes sure the
kernels are executed as efficiently as possible.")
    (license lgpl2.1+)))

(define-public starpu-1.2
  (package
    (inherit starpu-1.1)
    (name "starpu")
    (version "1.2.10")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url %starpu-git)
                   (commit (string-append "starpu-" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32 "1mlpc0zn1ra8b7whv1vfqxh6n935aj05c8n7sxgv8dky22bj56qw"))))
    (native-inputs
     ;; Some tests require bc and Gnuplot.
     `(("bc" ,bc)
       ("gnuplot" ,gnuplot)
       ,@(package-native-inputs starpu-1.1)))))

(define-public starpu-1.3
  (package
    (inherit starpu-1.2)
    (name "starpu")
    (version "1.3.4")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url %starpu-git)
                   (commit (string-append "starpu-" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32 "05s6l5hdvyk8m194cbg432r1qgxikzgn1s27xpci363c1yikgbxz"))
             (patches (search-patches %patch-path))))
   (arguments
    (substitute-keyword-arguments (package-arguments starpu-1.2)
      ((#:configure-flags flags '())
       (match (assoc "mpi" (package-propagated-inputs this-package))
         (("mpi" mpi)
          (if (string=? (package-name mpi) "nmad")
              `(cons "--enable-nmad" ,flags)
              flags))))
      ((#:phases phases '())
       (append phases '((add-after 'patch-source-shebangs 'fix-hardcoded-paths
                          (lambda _
                            (substitute* "min-dgels/base/make.inc"
                              (("/bin/sh")  (which "sh")))
                            #t)))))))
   (propagated-inputs  `(("mpi" ,openmpi)
                         ("hwloc" ,hwloc "lib")  ;hwloc 2.x
                         ,@(fold alist-delete
                                 (package-propagated-inputs starpu-1.2)
                                 '("mpi" "hwloc"))))))

(define-public starpu
  starpu-1.3)

(define-public starpu+fxt
  ;; When FxT support is enabled, performance is degraded, hence the separate
  ;; package.
  (package
    (inherit starpu)
    (name "starpu-fxt")
    (inputs `(("fxt" ,fxt)
              ,@(package-inputs starpu)))
    (arguments
     (substitute-keyword-arguments (package-arguments starpu)
       ((#:configure-flags flags '())
        `(cons "--with-fxt" ,flags))))
    ;; some tests require python.
    (native-inputs
     `(("python" ,python-2)
       ,@(package-native-inputs starpu)))))

(define-public starpu+simgrid
  (package
    (inherit starpu)
    (name "starpu-simgrid")
    (inputs `(("simgrid" ,simgrid)
              ,@(package-inputs starpu)))
    (arguments
     (substitute-keyword-arguments (package-arguments starpu)
       ((#:configure-flags flags '())
        `(cons "--enable-simgrid" ,flags))))))
