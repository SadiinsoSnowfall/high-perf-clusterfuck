;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019 Inria

(define-module (inria mpi)
  #:use-module (guix)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ssh)
  #:use-module (inria storm)
  #:use-module (srfi srfi-1))

(define-public hello-mpi
  (package
   (name "hello-mpi")
   (version "1.0.0")
   (home-page "https://gitlab.inria.fr/solverstack/hello-mpi.git")
   (synopsis "Hello world MPI")
   (description
    "This is a minimalist MPI hello world.")
   (license cecill-c)
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url home-page)
                  (commit "6d89892f0f279828010755883a56a65a822b8a2f")))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32
              "0aq33fqi94xmniwy2w25dkg1rv2jbawcjjqc8vjcznc5rd1ycybw"))))
   (arguments
    '(
      ;; #:configure-flags '("-DBUILD_SHARED_LIBS=ON"
      ;;                     "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE")
      #:phases (modify-phases %standard-phases
                             (add-before 'check 'prepare-test-environment
                                         (lambda _
                                           ;; Allow tests with more MPI processes than available CPU cores,
                                           ;; which is not allowed by default by OpenMPI
                                           (setenv "OMPI_MCA_rmaps_base_oversubscribe" "1") #t)))))
   (build-system cmake-build-system)
   (propagated-inputs `(("mpi" ,openmpi)
                        ("ssh" ,openssh)))))

(define-public openmpi-curta
  ;; Open MPI package matching the version of Open MPI on the Curta
  ;; super-computer.
  ;;
  ;; When creating Singularity images to run on Curta, one should use the same
  ;; version of Open MPI as the one available on Curta to avoid problems during
  ;; execution.
  ;;
  ;; See more: https://redmine.mcia.fr/projects/cluster-curta/wiki/Singularity
  (package
    (name "openmpi-curta")
    (version "3.1.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.open-mpi.org/software/ompi/v"
                          (version-major+minor version)
                          "/downloads/openmpi-" version ".tar.bz2"))
      (sha256
       (base32
        "1dks11scivgaskjs5955y9wprsl12wr3gn5r7wfl0l8gq03l7q4b"))))
    (build-system gnu-build-system)
    (inputs
     `(("hwloc" ,hwloc-1 "lib")
       ("gfortran" ,gfortran)
       ("libfabric" ,libfabric)
       ,@(if (and (not (%current-target-system))
                  (member (%current-system) (package-supported-systems psm)))
             `(("psm" ,psm))
             '())
       ,@(if (and (not (%current-target-system))
                  (member (%current-system) (package-supported-systems psm2)))
             `(("psm2" ,psm2))
             '())
       ("rdma-core" ,rdma-core)
       ("valgrind" ,valgrind)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)))
    (outputs '("out" "debug"))
    (arguments
     `(#:configure-flags `("--enable-mpi-ext=affinity" ;cr doesn't work
                           "--enable-memchecker"
                           "--with-sge"

                           ;; VampirTrace is obsoleted by scorep and disabling
                           ;; it reduces the closure size considerably.
                           "--disable-vt"

                           ,(string-append "--with-valgrind="
                                           (assoc-ref %build-inputs "valgrind"))
                           ,(string-append "--with-hwloc="
                                           (assoc-ref %build-inputs "hwloc")))
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'remove-absolute
                    (lambda _
                      ;; Remove compiler absolute file names (OPAL_FC_ABSOLUTE
                      ;; etc.) to reduce the closure size.  See
                      ;; <https://lists.gnu.org/archive/html/guix-devel/2017-07/msg00388.html>
                      ;; and
                      ;; <https://www.mail-archive.com/users@lists.open-mpi.org//msg31397.html>.
                      (substitute* '("orte/tools/orte-info/param.c"
                                     "oshmem/tools/oshmem_info/param.c"
                                     "ompi/tools/ompi_info/param.c")
                        (("_ABSOLUTE") ""))
                      ;; Avoid valgrind (which pulls in gdb etc.).
                      (substitute*
                          '("./ompi/mca/io/romio314/src/io_romio314_component.c")
                        (("MCA_io_romio314_COMPLETE_CONFIGURE_FLAGS")
                         "\"[elided to reduce closure]\""))
                      #t))
                  (add-before 'build 'scrub-timestamps ;reproducibility
                    (lambda _
                      (substitute* '("ompi/tools/ompi_info/param.c"
                                     "orte/tools/orte-info/param.c"
                                     "oshmem/tools/oshmem_info/param.c")
                        ((".*(Built|Configured) on.*") ""))
                      #t))
                  (add-after 'install 'remove-logs ;reproducibility
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (for-each delete-file (find-files out "config.log"))
                        #t))))))
    (home-page "http://www.open-mpi.org")
    (synopsis "MPI-3 implementation")
    (description
     "The Open MPI Project is an MPI-3 implementation that is developed and
maintained by a consortium of academic, research, and industry partners.  Open
MPI is therefore able to combine the expertise, technologies, and resources
from all across the High Performance Computing community in order to build the
best MPI library available.  Open MPI offers advantages for system and
software vendors, application developers and computer science researchers.")
    (license bsd-2)))

(define-public openmpi-with-mpi1-compat
  ;; In Open MPI 4 the deprecated MPI1 functions are disabled by default.
  ;; This variant enables them.
  (package
    (inherit openmpi)
    (name "openmpi-mpi1-compat")
    (arguments
     (substitute-keyword-arguments (package-arguments openmpi)
       ((#:configure-flags flags ''())
        `(cons "--enable-mpi1-compatibility" ,flags))))

    ;; Depend on hwloc 1.x because that's what users of this package expect.
    (inputs `(("hwloc" ,hwloc-1 "lib")
              ,@(alist-delete "hwloc" (package-inputs openmpi))))))
