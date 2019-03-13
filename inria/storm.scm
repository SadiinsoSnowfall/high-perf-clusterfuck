;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2018 Inria

(define-module (inria storm)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages autotools)
  #:use-module (inria storm-pm2)
  #:use-module (inria eztrace)
  #:use-module (inria simgrid)
  #:use-module (ice-9 match))

(define %starpu-home-page
  "http://starpu.gforge.inria.fr")

(define (starpu-url version)
  (string-append %starpu-home-page "/files/starpu-"
                 version "/starpu-" version ".tar.gz"))

(define %starpu-commit "1c35b01a4cdb79709adf275a48f5a4442bd1d875")
(define %starpu-git "https://scm.gforge.inria.fr/anonscm/git/starpu/starpu.git")

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
     `(("pkg-config" ,pkg-config)
       ("gdb" ,gdb)))                             ;used upon test failure
    (inputs `(("fftw" ,fftw)
              ("fftwf" ,fftwf)))
    (propagated-inputs  `(("hwloc" ,hwloc "lib")))
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
    (version "1.2.8")
    (source (origin
              (method url-fetch)
              (uri (starpu-url "1.2.8"))
              (sha256
               (base32
                "1yvmbax76lz4jg35xigab1i442xvvc77pwijy86x4j7jr3xnsrmn"))))
    (native-inputs
     ;; Some tests require bc and Gnuplot.
     `(("bc" ,bc)
       ("gnuplot" ,gnuplot)
       ,@(package-native-inputs starpu-1.1)))))


(define-public starpu
  (package
    (inherit starpu-1.2)
    (version "1.3.0rc1")
    (source (origin
              (method url-fetch)
              (uri "http://starpu.gforge.inria.fr/files/starpu-1.3.0rc1/starpu-1.3.0.tar.gz")
              (sha256
               (base32
                "12c84p0f4krf0bmfmdcfngp90qcpfzc8z4cq0pv98m0gy7asgzhy"))))
   (arguments
    (substitute-keyword-arguments (package-arguments starpu-1.2)
      ((#:phases phases '())
       (append phases '((add-after 'patch-source-shebangs 'fix-hardcoded-paths
                          (lambda _
                            (substitute* "min-dgels/base/make.inc"
                              (("/bin/sh")  (which "sh")))
                            #t)))))))))

;; TODO: build fails, implicit declaration of function ‘MPI_Type_hvector’
;; (define-public starpu+openmpi
;;   (package
;;     (inherit starpu)
;;     (name "starpu-openmpi")
;;     (inputs `(("mpi" ,openmpi)
;;               ,@(package-inputs starpu)))))

;; TODO: build fails, fatal error: boost/intrusive_ptr.hpp: No such file or directory
(define-public starpu+simgrid
  (package
    (inherit starpu-1.2)
    (name "starpu-simgrid")
    (inputs `(("simgrid" ,simgrid)
              ,@(package-inputs starpu-1.2)))
    (arguments
     (substitute-keyword-arguments (package-arguments starpu-1.2)
       ((#:configure-flags flags '())
        `(cons "--enable-simgrid" ,flags))))))

(define-public starpu+fxt
  ;; When FxT support is enabled, performance is degraded, hence the separate
  ;; package.
  (package
    (inherit starpu-1.2)
    (name "starpu-fxt")
    (inputs `(("fxt" ,fxt)
              ,@(package-inputs starpu-1.2)))
    (arguments
     (substitute-keyword-arguments (package-arguments starpu-1.2)
       ((#:configure-flags flags '())
        `(cons "--with-fxt" ,flags))))))

(define-public starpu-git
  (package
   (inherit starpu)
   (name "starpu-git")
   (version %starpu-commit)
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url %starpu-git)
                  (commit %starpu-commit)))
            (file-name (git-file-name name version))
            (sha256
             (base32 "0iqwyr5hd8i3h365r5vrnzjjyb3pf6kx7gyr3cph2jzj1d63cp2f"))))
   (native-inputs
    `(("libtool" ,libtool)
      ("autoconf" ,autoconf)
      ("automake" ,automake)
      ,@(package-native-inputs starpu)))))

(define-public starpu+nmad
  (package
   (inherit starpu-git)
   (name "starpu-nmad")
   (version %starpu-commit)
   (arguments
    (substitute-keyword-arguments (package-arguments starpu-git)
      ((#:configure-flags flags '())
       `(cons "--enable-nmad" ,flags))))
   (inputs
    `(("nmad" ,nmad)
      ,@(package-inputs starpu-git)))))

(define-public starpu+madmpi
  (package
   (inherit starpu+nmad)
   (name "starpu-madmpi")
   (arguments
    (substitute-keyword-arguments (package-arguments starpu+nmad)
                                  ((#:configure-flags flags '())
                                   `(delete "--enable-nmad" ,flags))))
   (inputs `(("nmad" ,nmad-mini)
             ,@(delete `("nmad" ,nmad) (package-inputs starpu+nmad))))))
