;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017-2022 Inria
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2014-2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2021 Franck Pérignon <franck.perignon@univ-grenoble-alpes.fr>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2017, 2018, 2019, 2020, 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>

(define-module (inria tadaam)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base))

(define %pm2-home-page "https://pm2.gitlabpages.inria.fr/")
(define %pm2-git "https://gitlab.inria.fr/pm2/pm2.git")
(define %pm2-commit "6aa6970d90c3cf754939de011690482a78575278")
(define %pm2-hash "0fg704d6wp4hlkwrac5gs9jkr8imp1ca60r4bssalv2zwdfm4jr6") ; guix hash -rx .

(define %v2022-11-22 "2022-11-22")

(define-public puk-2022-11-22
  (package
   (name "puk")
   (version %v2022-11-22)
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
   (native-inputs (list pkg-config autoconf automake))
   (propagated-inputs (list expat))
   (synopsis "PadicoTM micro-kernel")
   (description "Puk is the core of PadicoTM. It manages dynamically loadable
modules, software components, and basic data structures (lists, vectors,
hashtables, lock-free queues). It may be used with")
   (license license:gpl2)))

(define-public puk
  puk-2022-11-22)

(define-public pioman-2022-11-22
  (package
   (name "pioman")
   (version %v2022-11-22)
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
   (native-inputs (list pkg-config autoconf automake))
   (propagated-inputs (list puk `(,hwloc "lib")))
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
  pioman-2022-11-22)

(define-public pukabi-2022-11-22
  (package
   (name "pukabi")
   (version %v2022-11-22)
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
   (native-inputs (list pkg-config autoconf automake))
   (propagated-inputs (list puk))
   (synopsis "Dynamic ABI manager")
   (description "PukABI is a dynamic ABI manager. It intercepts symbols using
LD_PRELOAD to allow for a variety of features: replace a libc function with a
user-supplied function; add hooks for locking with another thread library
than libc pthread; add hooks for memory.")
   (license license:gpl2)))

(define-public pukabi
  pukabi-2022-11-22)

(define-public padicotm-2022-11-22
  (package
   (name "padicotm")
   (version %v2022-11-22)
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
   (inputs (list util-linux procps inetutils hwloc which))
   (native-inputs
    (list pkg-config autoconf automake `(,hwloc "lib") rdma-core psm psm2))
   (propagated-inputs (list puk pioman pukabi))
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
  padicotm-2022-11-22)

(define-public padicotm-mini-2022-11-22
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
  padicotm-mini-2022-11-22)

;;see comment above nmad*-pukabi packages definition
(define-public padicotm-pukabi-2022-11-22
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
  padicotm-pukabi-2022-11-22)

(define-public padicotm-mini-pukabi-2022-11-22
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
  padicotm-mini-pukabi-2022-11-22)

(define-public nmad-2022-11-22
  (package
   (name "nmad")
   (version %v2022-11-22)
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
                 (add-after 'install 'install-examples-too
                            (lambda _
                            (invoke "make" "-j" (number->string (parallel-job-count)) "-C" "examples" "install")))
                 (delete 'check))))
   (native-inputs
    (list pkg-config autoconf automake gfortran
          `(,hwloc "lib")))
   (propagated-inputs
    (list padicotm))
   (inputs
    (list rdma-core psm psm2))
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
  nmad-2022-11-22)

(define-public nmad-mini-2022-11-22
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
  nmad-mini-2022-11-22)

;;nmad-pukabi and nmad-mini-pukabi corresponds to old packages that were not using pukabi
;;they should only be used in case something goes wrong with the default ones
;;they are not meant to be maintained
(define-public nmad-pukabi-2022-11-22
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

(define-public nmad-mini-pukabi-2022-11-22
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

(define-public mpibenchmark
  (package
   (name "mpibenchmark")
   (version "0.4")
   (home-page (string-append %pm2-home-page "mpibenchmark"))
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url %pm2-git)
                  (commit "9f08955628deca25e45bb23be21f91e7d0377121")))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32 "0pk872qdjvw9jx6rnilxlzyp0ygbgcamh38c5izvbkh9pms00l4h"))))
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
   (native-inputs (list pkg-config autoconf automake))
   (inputs
    (list `(,hwloc "lib") gnuplot nmad))
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

(define-public mpi_sync_clocks-2022-11-22
  (package
   (name "mpi_sync_clocks")
   (version %v2022-11-22)
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
    (list pkg-config autoconf automake))
   (inputs
    (list openmpi)) ; Every packet requiring mpi use openmpi, so use it, it will be simpler to then change with `--with-input=openmpi=nmad`
   (synopsis "Distributed synchronized clocks over MPI")
   (description "Small library with routines to synchronize clocks over several
                nodes with MPI.")
   (license license:lgpl2.1)))

(define-public topomatch
  (package
    (name "topomatch")
    (version "1.1")
    (home-page "https://gitlab.inria.fr/ejeannot/topomatch")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "V" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1n0nfs1kmlq9bf1lvijf04vryslpv8cq99gn96rpmw8sjx7jbj14"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config
                         autoconf
                         automake
                         libtool
                         `(,hwloc "lib")
                         perl))
    (propagated-inputs (list scotch-6))
    (synopsis "Process mapping algorithms and tools for general topologies")
    (description
     "TopoMatch leverages on the Scotch library to handle any type of topologies and not only trees.
Its main features are:
- Handling any type of topologies (tgt Scotch format or hwloc format).
- Handle large communication patterns (up to hundreds of thousands of processes and processing units) .
- Manage binding constraints: you can specify a subset of the node onto which you want to do the mapping.
- Manage oversubscribing: you can specify that more than one processes can be mapped onto a each processing unit.
- Deal with logical numbering. Physical core numbering can be used with XML/HWLOC topologies.
- Provide exhaustive search for small cases.
- Adaptive algorithmic that provide a good trade-off between quality and speed.
- Crucial sections of the code are multithreaded.
- Optimize I/O to read large input files.
- Portable on Unix-like systems (Linux, OS-X, etc.).
- Many useful options (level of verbosity, topology optimization, partitioning, etc.).")
    (license license:bsd-3)))

(define-public mpi_sync_clocks
  mpi_sync_clocks-2022-11-22)

(define-public scotch-6
  ;; Copied from Guix commit 4c1dff9abeb383ca58dbfcbc27e1bd464d2ad2ea.
  (package
    (inherit scotch)
    (version "6.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.labri.fr/perso/pelegrin/scotch/distrib/scotch_"
                    version ".tar.gz"))
              (sha256
               (base32
                "04dkz24a2g20wq703fnyi4440ac4mwycy9gwrrllljj7zxcjy19r"))
              (patches (search-patches
                        "inria/patches/scotch-build-parallelism.patch"
                        "inria/patches/scotch-integer-declarations.patch"))))
    (build-system gnu-build-system)
    (inputs
     (list zlib))
    (native-inputs
     (list flex bison gfortran))
    (outputs '("out" "metis"))
    (arguments
     `(#:make-flags (list (string-append "prefix=" %output))
       #:phases
       (modify-phases %standard-phases
         (add-after
             'unpack 'chdir-to-src
           (lambda _ (chdir "src") #t))
         (replace
             'configure
           (lambda _
             (call-with-output-file "Makefile.inc"
               (lambda (port)
                 (format port "
EXE =
LIB = .a
OBJ = .o
MAKE = make
AR = ar
ARFLAGS = -ruv
CAT = cat
CCS = gcc
CCP = mpicc
CCD = gcc
FC = gfortran
CPPFLAGS =~{ -D~a~}
CFLAGS = -O2 -g -fPIC $(CPPFLAGS)
LDFLAGS = -lz -lm -lrt -lpthread
CP = cp
LEX = flex -Pscotchyy -olex.yy.c
LN = ln
MKDIR = mkdir
MV = mv
RANLIB = ranlib
YACC = bison -pscotchyy -y -b y
"
                         '("COMMON_FILE_COMPRESS_GZ"
                           "COMMON_PTHREAD"
                           "COMMON_RANDOM_FIXED_SEED"
                           "INTSIZE64"            ;use 'int64_t'
                           ;; Prevents symbol clashes with libesmumps
                           "SCOTCH_RENAME"
                           ;; XXX: Causes invalid frees in superlu-dist tests
                           ;; "SCOTCH_PTHREAD"
                           ;; "SCOTCH_PTHREAD_NUMBER=2"
                           "restrict=__restrict"))))
             #t))
         (add-after 'build 'build-esmumps
           (lambda _
             (invoke "make"
                     (format #f "-j~a" (parallel-job-count))
                     "esmumps")))
         (add-before 'install 'make-install-dirs
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir (assoc-ref outputs "out"))))
         (add-after 'install 'install-metis
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "metis")))
               (mkdir out)
               ;; metis files are not installed with 'make install'
               (for-each (lambda (f)
                           (install-file f (string-append out "/include")))
                         (find-files "../include/" ".*metis\\.h"))
               (for-each (lambda (f)
                           (install-file f (string-append out "/lib")))
                         (find-files "../lib/" ".*metis\\..*"))
               #t))))))))

(define-public pt-scotch-6
  (package
    (inherit scotch-6)
    (name "pt-scotch")
    (propagated-inputs
     (list openmpi))                             ;headers include MPI headers
    (arguments
     (substitute-keyword-arguments (package-arguments scotch-6)
       ((#:phases scotch-phases)
        `(modify-phases ,scotch-phases
           (replace
            'build
            (lambda _
              (invoke "make" (format #f "-j~a" (parallel-job-count))
                      "ptscotch" "ptesmumps")

              ;; Install the serial metis compatibility library
              (invoke "make" "-C" "libscotchmetis" "install")))
           (add-before 'check 'mpi-setup
	     ,%openmpi-setup)
           (replace 'check
             (lambda _
               (invoke "make" "ptcheck")))))))
    (synopsis "Programs and libraries for graph algorithms (with MPI)")))
