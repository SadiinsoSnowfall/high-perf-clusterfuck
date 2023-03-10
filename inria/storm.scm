;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017-2022 Inria

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
  #:use-module (gnu packages llvm)
  #:use-module (inria tadaam)
  #:use-module (inria eztrace)
  #:use-module (inria mpi)
  #:use-module (inria simgrid)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define %starpu-home-page
  "https://starpu.gitlabpages.inria.fr/")

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
     (list gfortran
           pkg-config
           gdb
           libtool
           autoconf
           automake))                             ;used upon test failure
    (inputs (list fftw fftwf))
    (propagated-inputs  (list `(,hwloc-1 "lib") openmpi-with-mpi1-compat))
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
     (modify-inputs (package-native-inputs starpu-1.1)
       (prepend bc gnuplot)))))

(define (starpu-configure-flags package)
  ;; Return the standard configure flags for PACKAGE, a StarPU 1.3+ package.
  `(list "--enable-quick-check"
         ,@(if (assoc "fxt" (package-inputs package)) ;optional fxt dependency
               '("--with-fxt")
               '())
         ,@(if (assoc "simgrid" (package-inputs package)) ;optional simgrid
               '("--enable-simgrid"
                 "--enable-maxcpus=1000"
                 "--enable-maxcudadev=1000"
                 "--enable-maxnodes=32")
               ;; For actual runs (simgrid OFF), we fix the maximum number of
               ;; CPU workers to 128 instead of letting starpu decide that
               ;; wrt to the actual topology it is built on, in order to
               ;; ensure a portable and reproducible cross-compilation. In
               ;; the future, it would be nice to give the opportunity to
               ;; change it at will when parametrized packages will be there.
               '("--enable-maxcpus=128"))
         ,@(match (assoc "mpi" (package-propagated-inputs package))
             (("mpi" mpi)
              (if (string=? (package-name mpi) "nmad")
                  '("--enable-nmad")
                  '()))
             (#f
              '()))))

(define-public starpu-1.3
  (package
    (inherit starpu-1.2)
    (name "starpu")
    (version "1.3.10")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url %starpu-git)
                   (commit (string-append "starpu-" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32 "1id5r3krcab48vcwxbk4z5ywddv5a0s5vmncnh4v1sq6qbnkjlq7"))
             (patches (search-patches %patch-path))))
   (arguments
    (substitute-keyword-arguments (package-arguments starpu-1.2)
      ((#:configure-flags _ '())
       (starpu-configure-flags this-package))
      ((#:phases phases '())
       (append phases '((add-after 'patch-source-shebangs 'fix-hardcoded-paths
                          (lambda _
                            (substitute* "min-dgels/base/make.inc"
                              (("/bin/sh")  (which "sh")))
                            #t)))))))
   (propagated-inputs  (modify-inputs (package-propagated-inputs starpu-1.2)
                         (delete "mpi" "hwloc")
                         (prepend openmpi
                                  `(,hwloc "lib") ;hwloc 2.x
                                  )))))

; next release of StarPU will have an optional dependency on tadaam/mpi_sync_clocks: don't forget to add it !

(define-public starpu
  starpu-1.3)

(define-public starpu+fxt
  ;; When FxT support is enabled, performance is degraded, hence the separate
  ;; package.
  (package
    (inherit starpu)
    (name "starpu-fxt")
    (inputs (modify-inputs (package-inputs starpu)
              (prepend fxt)))
    ;; some tests require python.
    (native-inputs
     (modify-inputs (package-native-inputs starpu)
       (prepend python-wrapper)))))

(define-public starpu+simgrid
  (package
    (inherit starpu)
    (name "starpu-simgrid")
    (arguments
     (substitute-keyword-arguments (package-arguments starpu)
       ((#:configure-flags flags '())
        `(cons "--enable-simgrid" (cons "--enable-mpi" (cons "--disable-shared" ,flags))))))
    (inputs (modify-inputs (package-inputs starpu)
              (prepend simgrid fxt+static)))
    (propagated-inputs `(,@(delete `("mpi" ,openmpi) (package-propagated-inputs starpu))))
    ;; some tests require python.
    (native-inputs
     (modify-inputs (package-native-inputs starpu)
       (prepend python-wrapper)))))

(define-public parcoach-1.2
  (package
    (name "parcoach")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/parcoach/parcoach")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "14l90xddz8qx6jp7dkvys1k8mdg2dp5jwg62y94y8i7yx0qi4pxi"))))
    (build-system cmake-build-system)
    (native-inputs
     (list clang-9 clang-toolchain-9 python-3))
    (inputs
     (list llvm-9 openmpi))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'substitute-script
           (lambda* (#:key source inputs outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "out"))
                   (llvm (assoc-ref inputs "llvm")))
               (copy-file (string-append source "/src/aSSA/parcoach.in")
                          "parcoach.in")
               (substitute* "parcoach.in"
                 (("@LLVM_TOOLS_BINARY_DIR@")
                  (string-append llvm "/bin")))
               (substitute* "parcoach.in"
                 (("@PARCOACH_LIB@")
                  (string-append out "/lib/aSSA.so"))))))
         (add-after 'install 'install-script
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out      (assoc-ref outputs "out"))
                    (out-bin  (string-append out "/bin"))
                    (parcoach (string-append out-bin "/parcoach")))
               (mkdir out-bin)
               (copy-file "parcoach.in" parcoach)))))))
    (synopsis "Analysis tool for errors detection in parallel
applications")
    (description "PARCOACH is an Open-source software dedicated to the
collective errors detection in parallel applications.")
    (home-page "https://parcoach.github.io/")
    (license lgpl2.1)))

(define-public parcoach
  parcoach-1.2)
