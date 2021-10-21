;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017-2021 Inria

(define-module (inria tadaam)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base))

(define %pm2-home-page "https://pm2.gitlabpages.inria.fr/")
(define %pm2-git "https://gitlab.inria.fr/pm2/pm2.git")
(define %pm2-commit "2765619bd2c77af733778643f369ba086a29715a")
(define %pm2-hash "02fwq63wmljk3d1val4y46jkr6lvdbd6d6qkkkpar51r9qf5n5h1") ; guix hash -rx .

(define %v2021-05-21 "2021-05-21")

(define-public puk-2021-05-21
  (package
   (name "puk")
   (version %v2021-05-21)
   (home-page (string-append %pm2-home-page "PadicoTM"))
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url %pm2-git)
                  (commit %pm2-commit)))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32 %pm2-hash))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
                          "--disable-debug"
                          "--disable-trace")
      #:phases (modify-phases %standard-phases
                 (add-after 'unpack 'fix-hardcoded-paths-chdir
                   (lambda _
                     (substitute* "building-tools/common_vars.mk.in"
                       (("/bin/sh")  (which "sh")))
                     (chdir "Puk")
                     #t))
                 (delete 'check)))) ; no make check in Puk
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf", autoconf)
      ("automake", automake)))
   (propagated-inputs
    `(("expat" ,expat)))
   (synopsis "PadicoTM micro-kernel")
   (description "Puk is the core of PadicoTM. It manages dynamically loadable
modules, software components, and basic data structures (lists, vectors,
hashtables, lock-free queues). It may be used with")
   (license license:gpl2)))

(define-public puk
  puk-2021-05-21)

(define-public pioman-2021-05-21
  (package
   (name "pioman")
   (version %v2021-05-21)
   (home-page (string-append %pm2-home-page "pioman"))
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url %pm2-git)
                  (commit %pm2-commit)))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32 %pm2-hash))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
                          "--disable-debug"
                          "--with-pthread")
      #:phases (modify-phases %standard-phases
                 (add-after 'unpack 'fix-hardcoded-paths-chdir
                   (lambda _
                     (substitute* "building-tools/common_vars.mk.in"
                       (("/bin/sh")  (which "sh")))
                     (chdir "pioman")
                     #t))
                 (delete 'check)))) ; no make check in pioman
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf" ,autoconf)
      ("automake" ,automake)))
   (propagated-inputs
    `(("puk" ,puk)
      ("hwloc" ,hwloc "lib")))
   (synopsis "A Generic I/O Manager")
   (description " PIOMan is an I/O event manager of the PM2 software suite. It
ensures communication progression using available cores and hooks in thread
scheduler. It guarantees good reactivity, asynchronous communication progression,
and communication/computation overlap.
PIOMan is closely integrated with the NewMadeleine communication library and
PadicoTM. It works with three flavors of thread scheduling: no thread, pthread,
and Marcel. The pthread flavor may be composed with various runtime systems such
as OpenMP.
PIOMan can be used standalone to bring low level asynchronous progression in a
communication library, or more simply may be used through the NewMadeleine
communication library and its companion MPI implementation called Mad-MPI
supporting MPI_THREAD_MULTIPLE multi-threading level.")
   (license license:gpl2)))

(define-public pioman
  pioman-2021-05-21)

(define-public pukabi-2021-05-21
  (package
   (name "pukabi")
   (version %v2021-05-21)
   (home-page (string-append %pm2-home-page "PadicoTM"))
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url %pm2-git)
                  (commit %pm2-commit)))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32 %pm2-hash))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
                          "--disable-debug"
                          "--enable-mem")
      #:phases (modify-phases %standard-phases
                 (add-after 'unpack 'fix-hardcoded-paths-chdir
                   (lambda _
                     (substitute* "building-tools/common_vars.mk.in"
                       (("/bin/sh")  (which "sh")))
                     (chdir "PukABI")
                     #t))
                 (delete 'check))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf", autoconf)
      ("automake", automake)))
   (propagated-inputs
    `(("puk" ,puk)))
   (synopsis "Dynamic ABI manager")
   (description "PukABI is a dynamic ABI manager. It intercepts symbols using
LD_PRELOAD to allow for a variety of features: replace a libc function with a
user-supplied function; add hooks for locking with another thread library
than libc pthread; add hooks for memory.")
   (license license:gpl2)))

(define-public pukabi
  pukabi-2021-05-21)

(define-public padicotm-2021-05-21
  (package
   (name "padicotm")
   (version %v2021-05-21)
   (home-page (string-append %pm2-home-page "PadicoTM"))
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url %pm2-git)
                  (commit %pm2-commit)))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32 %pm2-hash))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
                          "--disable-debug"
                          "--with-pioman"

                          ;; 'padico-d' wants to write to $localstatedir/log.
                          "--localstatedir=/var")
      #:phases (modify-phases %standard-phases
                 (add-after 'unpack 'fix-hardcoded-paths-chdir
                   (lambda _
                     (substitute* "building-tools/common_vars.mk.in"
                       (("/bin/sh")  (which "sh")))
                     (chdir "PadicoTM")
                     #t))
                 (delete 'check)
                 (add-after 'install 'wrap-padico-launch
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     ;; Wrap the 'padico-launch' shell script so that it
                     ;; finds all the commands that it needs.
                     (define (input-directory input)
                       (string-append (assoc-ref inputs input)
                                      "/bin"))

                     (let* ((path (map input-directory
                                       '("util-linux" ;'setsid'
                                         "inetutils"  ;'hostname'
                                         "procps"     ;'ps'
                                         "hwloc"      ;for 'padico-d'
                                         "which"
                                         "tar"
                                         "gzip"
                                         "coreutils"
                                         "grep"
                                         "sed"
                                         "gawk")))
                            (out  (assoc-ref outputs "out"))
                            (bin  (string-append out "/bin")))
                       (wrap-program (string-append bin "/padico-launch")
                         `("PATH" ":" prefix ,path))
                       #t))))))
   (inputs
    `(("util-linux" ,util-linux)
      ("procps" ,procps)
      ("inetutils" ,inetutils)
      ("hwloc" ,hwloc)
      ("which" ,which)))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf" ,autoconf)
      ("automake" ,automake)
      ("hwloc:lib" ,hwloc "lib")
      ("rdma-core" ,rdma-core)
      ("psm" ,psm)
      ("psm2" ,psm2)))
   (propagated-inputs
    `(("puk" ,puk)
      ("pioman" ,pioman)
      ("pukabi" ,pukabi)))
   (synopsis "A High-performance Communication Framework for Grids")
   (description "PadicoTM is composed of a core which provides a
high-performance framework for networking and multi-threading, and
services plugged into the core. High-performance communications
and threads are obtained thanks to Marcel and Madeleine, provided
by the PM2 software suite. The PadicoTM core aims at making the
different services running at the same time run in a cooperative
way rather than competitive.")
   (license license:gpl2)))

(define-public padicotm
  padicotm-2021-05-21)

(define-public padicotm-mini-2021-05-21
  (package
   (inherit padicotm)
   (name "padicotm-mini")
   (arguments
    (substitute-keyword-arguments (package-arguments padicotm)
      ((#:configure-flags flags '())
       `(cons "--without-pioman" (delete "--with-pioman" ,flags)))))
   (propagated-inputs
    `(,@(delete `("pioman" ,pioman) (package-propagated-inputs padicotm))))))

(define-public padicotm-mini
  padicotm-mini-2021-05-21)

;;see comment above nmad*-pukabi packages definition
(define-public padicotm-pukabi-2021-05-21
  (package
   (inherit padicotm)
   (name "padicotm-pukabi")
   (arguments
    (substitute-keyword-arguments (package-arguments padicotm)
      ((#:configure-flags flags '())
       `(cons "--without-pukabi"  ,flags))))
   (propagated-inputs
    `(,@(delete `("pukabi" ,pukabi) (package-propagated-inputs padicotm))))))

(define-public padicotm-pukabi
  padicotm-pukabi-2021-05-21)

(define-public padicotm-mini-pukabi-2021-05-21
  (package
   (inherit padicotm-mini)
   (name "padicotm-mini-pukabi")
   (arguments
    (substitute-keyword-arguments (package-arguments padicotm-mini)
      ((#:configure-flags flags '())
       `(cons "--without-pukabi" ,flags))))
   (propagated-inputs
    `(,@(delete `("pukabi" ,pukabi) (package-propagated-inputs padicotm-mini))))))

(define-public padicotm-mini-pukabi
  padicotm-mini-pukabi-2021-05-21)

(define-public nmad-2021-05-21
  (package
   (name "nmad")
   (version %v2021-05-21)
   (home-page (string-append %pm2-home-page "NewMadeleine"))
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url %pm2-git)
                  (commit %pm2-commit)))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32 %pm2-hash))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
                          "--disable-debug"
                          "--with-pioman"
                          "--with-pukabi"
                          "--enable-mpi"
                          "--disable-sampling")
      #:phases (modify-phases %standard-phases
                  ;(add-before 'check 'pre-check
                    ;(lambda _
                      ;(setenv "PADICO_VERBOSE" "yes") ; for verbose tests
                      ;#t))
                 (add-after 'unpack 'fix-hardcoded-paths-chdir
                   (lambda _
                     (substitute* "building-tools/common_vars.mk.in"
                       (("/bin/sh")  (which "sh")))
                     (chdir "nmad")
                     #t))
                 (add-after 'install 'set-libexec-dir-mpicc
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out")))
                       (for-each (lambda (file)
                                   (substitute* file
                                     (("^libexec=.*")
                                      (string-append "libexec=" out
                                                     "/libexec\n"))))
                                 (find-files (string-append out "/bin")
                                             "^mpi"))
                       #t)))
                 (delete 'check))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf" ,autoconf)
      ("automake" ,automake)
      ("gfortran" ,gfortran)
      ("hwloc" ,hwloc "lib")))
   (propagated-inputs
    `(("padicotm" ,padicotm)))
   (inputs
    `(("rdma-core" ,rdma-core)
      ("psm" ,psm)
      ("psm2" ,psm2)))
   (synopsis "An Optimizing Communication Library for High-Performance Networks")
   (description "NewMadeleine is the fourth incarnation of the Madeleine
communication library. The new architecture aims at enabling the use of a much
wider range of communication flow optimization techniques. Its design is entirely
modular: drivers and optimization strategies are dynamically loadable software
components, allowing experimentations with multiple approaches or on multiple
issues with regard to processing communication flows.
The optimizing scheduler SchedOpt targets applications with irregular, multi-flow
communication schemes such as found in the increasingly common application
conglomerates made of multiple programming environments and coupled pieces of
code, for instance. SchedOpt itself is easily extensible through the concepts of
optimization strategies (what to optimize for, what the optimization goal is)
expressed in terms of tactics (how to optimize to reach the optimization goal).
Tactics themselves are made of basic communication flows operations such as packet
merging or reordering.
The communication library is fully multi-threaded through its close integration
with PIOMan. It manages concurrent communication operations from multiple
libraries and from multiple threads. Its MPI implementation Mad-MPI fully supports
the MPI_THREAD_MULTIPLE multi-threading level.")
   (license license:gpl2)))

(define-public nmad
  nmad-2021-05-21)

(define-public nmad-mini-2021-05-21
  (package
   (inherit nmad)
   (name "nmad-mini")
   (arguments
    (substitute-keyword-arguments (package-arguments nmad)
      ((#:configure-flags flags '())
       `(cons "--without-pioman" (delete "--with-pioman" ,flags)))))
   (propagated-inputs
    `(("padicotm" ,padicotm-mini)
      ,@(delete `("padicotm" ,padicotm) (package-propagated-inputs nmad))))))

(define-public nmad-mini
  nmad-mini-2021-05-21)

;;nmad-pukabi and nmad-mini-pukabi corresponds to old packages that were not using pukabi
;;they should only be used in case something goes wrong with the default ones
;;they are not meant to be maintained
(define-public nmad-pukabi-2021-05-21
  (package
   (inherit nmad)
   (name "nmad-pukabi")
   (arguments
    (substitute-keyword-arguments (package-arguments nmad)
      ((#:configure-flags flags '())
       `(cons "--without-pukabi" (delete "--with-pukabi" ,flags)))))
   (propagated-inputs
    `(("padicotm" ,padicotm-pukabi)
      ,@(delete `("padicotm" ,padicotm) (package-propagated-inputs nmad))))))

(define-public nmad-mini-pukabi-2021-05-21
  (package
   (inherit nmad-mini)
   (name "nmad-mini-pukabi")
   (arguments
    (substitute-keyword-arguments (package-arguments nmad-mini)
      ((#:configure-flags flags '())
       `(cons "--without-pukabi" (delete "--with-pukabi" ,flags)))))
   (propagated-inputs
    `(("padicotm" ,padicotm-mini-pukabi)
      ,@(delete `("padicotm" ,padicotm-mini) (package-propagated-inputs nmad-mini))))))

(define-public mpibenchmark-2021-05-21
  (package
   (name "mpibenchmark")
   (version %v2021-05-21)
   (home-page (string-append %pm2-home-page "mpibenchmark"))
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url %pm2-git)
                  (commit %pm2-commit)))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32 %pm2-hash))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
                          "--disable-debug")
      #:phases (modify-phases %standard-phases
                 (add-after 'unpack 'fix-hardcoded-paths-chdir
                   (lambda _
                     (substitute* "building-tools/common_vars.mk.in"
                       (("/bin/sh")  (which "sh")))
                     (substitute* "mpi_sync_clocks/autogen.sh"
                       (("/bin/sh")  (which "sh")))
                     (chdir "mpibenchmark")
                     #t))
                 (delete 'check)))) ; no make check in mpibenchmark
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf" ,autoconf)
      ("automake" ,automake)))
   (inputs
    `(("hwloc" ,hwloc "lib")
      ("gnuplot" ,gnuplot)
      ("mpi" ,nmad)))
   (synopsis "MPI overlap benchmark")
   (description "MadMPI benchmark contains the following benchmark series:
- base send/recv benchmark, used for reference (mpi_bench_base);
- communication/computation overlap benchmark (mpi_bench_overlap);
- tag-matching performance with tags of posted receives in order and out of
  order (mpi_bench_reqs);
- multi-threaded communications benchmark (mpi_bench_thread) // preliminary
  version, still incomplete.
Benchmarks are point-to-point, running on two nodes. Collective operations
are not benchmarked yet.")
   (license license:gpl2)))

(define-public mpibenchmark
  mpibenchmark-2021-05-21)

(define-public mpi_sync_clocks-2021-05-21
  (package
   (name "mpi_sync_clocks")
   (version %v2021-05-21)
   (home-page %pm2-home-page)
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url %pm2-git)
                  (commit %pm2-commit)))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32 %pm2-hash))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
                          "--disable-debug")
      #:phases (modify-phases %standard-phases
                 (add-after 'unpack 'fix-hardcoded-paths-chdir
                   (lambda _
                     (substitute* "building-tools/common_vars.mk.in"
                       (("/bin/sh")  (which "sh")))
                     (chdir "mpi_sync_clocks")
                     #t))
                 (delete 'check)))) ; no make check in mpi_sync_clocks
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf" ,autoconf)
      ("automake" ,automake)))
   (inputs
    `(("mpi" ,openmpi))) ; Every packet requiring mpi use openmpi, so use it, it will be simpler to then change with `--with-input=openmpi=nmad`
   (synopsis "Distributed synchronized clocks over MPI")
   (description "Small library with routines to synchronize clocks over several
                nodes with MPI.")
   (license license:lgpl2.1)))

(define-public mpi_sync_clocks
  mpi_sync_clocks-2021-05-21)
