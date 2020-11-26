;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2018, 2019, 2020 Inria

(define-module (inria tadaam)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module (guix svn-download)
  #:use-module ((guix licenses) #:prefix license:)
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
  #:use-module (gnu packages base)
  #:use-module (inria eztrace)
  #:use-module (inria simgrid)
  #:use-module (ice-9 match))

(define %pm2-home-page "http://pm2.gforge.inria.fr/")
(define %pm2-svn "https://scm.gforge.inria.fr/anonscm/svn/pm2/trunk")
(define %padicotm-svn "https://scm.gforge.inria.fr/anonscm/svn/padico/PadicoTM/trunk")
(define %patch-path "inria/patches/autogen_building-tools.patch")

(define %v2020-11-26 "2020-11-26")
(define %v2020-11-26-pm2-revision 29045)
(define %v2020-11-26-padicotm-revision 5464)

(define-public puk-2020-11-26
  (package
   (name "puk")
   (version %v2020-11-26)
   (home-page (string-append %pm2-home-page "/PadicoTM"))
   (source (origin
            (method svn-fetch)
            (uri (svn-reference
                  (url (string-append %padicotm-svn "/PadicoTM/Puk"))
                  (revision %v2020-11-26-padicotm-revision)))
            (sha256
             (base32 "0fgjvsq2mbx7fj00vd968832rr9fny4d195cs834kl39cl2jlfz9"))
            (patches (search-patches %patch-path))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
                          "--disable-debug"
                          "--disable-trace")
      #:phases (modify-phases %standard-phases
                 (add-after 'unpack 'fix-hardcoded-paths
                   (lambda _
                     (substitute* "building-tools/common_vars.mk.in"
                       (("/bin/sh")  (which "sh")))
                     #t))
                 (delete 'check))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf", autoconf)
      ("automake", automake)))
   (propagated-inputs
    `(("expat" ,expat)))
   (synopsis "dummy")
   (description "Dummy")
   (license license:lgpl2.0)))

(define-public puk
  puk-2020-11-26)

(define-public pioman-2020-11-26
  (package
   (name "pioman")
   (version %v2020-11-26)
   (home-page (string-append %pm2-home-page "/pioman"))
   (source (origin
            (method svn-fetch)
            (uri (svn-reference
                  (url (string-append %pm2-svn "/pioman"))
                  (revision %v2020-11-26-pm2-revision)))
            (sha256
             (base32 "065y87ppi72dz7lqddk2ff9bmj2dv4j647af2gd3pqlc2nv0acfz"))
            (patches (search-patches %patch-path))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
                          "--disable-debug"
                          "--with-pthread")
      #:phases (modify-phases %standard-phases
                 (add-after 'unpack 'fix-hardcoded-paths
                   (lambda _
                     (substitute* "building-tools/common_vars.mk.in"
                       (("/bin/sh")  (which "sh")))
                     #t))
                 (delete 'check))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf" ,autoconf)
      ("automake" ,automake)))
   (propagated-inputs
    `(("puk" ,puk)
      ("hwloc" ,hwloc "lib")))
   (synopsis "dummy")
   (description "Dummy")
   (license license:lgpl2.0)))

(define-public pioman
  pioman-2020-11-26)

(define-public pukabi-2020-11-26
  (package
   (name "pukabi")
   (version %v2020-11-26)
   (home-page (string-append %pm2-home-page "/PadicoTM"))
   (source (origin
            (method svn-fetch)
            (uri (svn-reference
                  (url (string-append %padicotm-svn "/PadicoTM/PukABI"))
                  (revision %v2020-11-26-padicotm-revision)))
            (sha256
             (base32 "0d8jscxbz67vr53v0v0r8amw0q6qgv8qrq43nwjkj56z9alz6ah8"))
            (patches (search-patches %patch-path))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
                          "--disable-debug"
			  "--enable-mem")
      #:phases (modify-phases %standard-phases
                 (add-after 'unpack 'fix-hardcoded-paths
                   (lambda _
                     (substitute* "building-tools/common_vars.mk.in"
                       (("/bin/sh")  (which "sh")))
                     #t))
                 (delete 'check))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf", autoconf)
      ("automake", automake)))
   (propagated-inputs
    `(("puk" ,puk)))
   (synopsis "dummy")
   (description "Dummy")
   (license license:lgpl2.0)))

(define-public pukabi
  pukabi-2020-11-26)

(define-public padicotm-2020-11-26
  (package
   (name "padicotm")
   (version %v2020-11-26)
   (home-page (string-append %pm2-home-page "/PadicoTM"))
   (source (origin
            (method svn-fetch)
            (uri (svn-reference
                  (url (string-append %padicotm-svn "/PadicoTM/PadicoTM"))
                  (revision %v2020-11-26-padicotm-revision)))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32 "1a2daga6r9xcrm6nrmmnv1p0p60z30s1vd30jklv0p9ajy1mgy9l"))
            (patches (search-patches %patch-path))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
                          "--disable-debug"
                          "--with-pioman"

                          ;; 'padico-d' wants to write to $localstatedir/log.
                          "--localstatedir=/var")
      #:phases (modify-phases %standard-phases
                 (add-after 'unpack 'fix-hardcoded-paths
                   (lambda _
                     (substitute* "building-tools/common_vars.mk.in"
                       (("/bin/sh")  (which "sh")))
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
                                         "tar" "gzip"
                                         "coreutils" "grep" "sed")))
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
   (synopsis "dummy")
   (description "Dummy")
   (license license:lgpl2.0)))

(define-public padicotm
  padicotm-2020-11-26)

(define-public padicotm-mini-2020-11-26
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
  padicotm-mini-2020-11-26)

;;see comment above nmad*-pukabi packages definition
(define-public padicotm-pukabi-2020-11-26
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
  padicotm-pukabi-2020-11-26)

(define-public padicotm-mini-pukabi-2020-11-26
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
  padicotm-mini-pukabi-2020-11-26)

(define-public nmad-2020-11-26
  (package
   (name "nmad")
   (version %v2020-11-26)
   (home-page (string-append %pm2-home-page "/NewMadeleine"))
   (source (origin
            (method svn-fetch)
            (uri (svn-reference
                  (url (string-append %pm2-svn "/nmad"))
                  (revision %v2020-11-26-pm2-revision)))
            (sha256
             (base32 "103k9523ikc8y5gahkd20l4pv5sb2qq2vcm6ixjc2dyid8wp7wq2"))
            (patches (search-patches %patch-path))))
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
                 (add-after 'unpack 'fix-hardcoded-paths
                   (lambda _
                     (substitute* "building-tools/common_vars.mk.in"
                       (("/bin/sh")  (which "sh")))
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
   (synopsis "dummy")
   (description "Dummy")
   (license license:lgpl2.0)))

(define-public nmad
  nmad-2020-11-26)

(define-public nmad-mini-2020-11-26
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
  nmad-mini-2020-11-26)

;;nmad-pukabi and nmad-mini-pukabi corresponds to old packages that were not using pukabi
;;they should only be used in case something goes wrong with the default ones
;;they are not meant to be maintained
(define-public nmad-pukabi-2020-11-26
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

(define-public nmad-mini-pukabi-2020-11-26
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

(define-public mpibenchmark-2020-11-26
  (package
   (name "mpibenchmark")
   (version %v2020-11-26)
   (home-page (string-append %pm2-home-page "/mpibenchmark"))
   (source (origin
            (method svn-fetch)
            (uri (svn-reference
                  (url (string-append %pm2-svn "/mpibenchmark"))
                  (revision %v2020-11-26-pm2-revision)))
            (sha256
             (base32 "1bm1xagi7nnd09g9vrdijbs7gacsja3cl3h2091bkzh376zfvl51"))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
                          "--disable-debug")
      #:phases (modify-phases %standard-phases
                 (add-after 'unpack 'fix-hardcoded-paths
                   (lambda _
                     (substitute* "building-tools/common_vars.mk.in"
                       (("/bin/sh")  (which "sh")))
                     #t))
                 (delete 'check))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf" ,autoconf)
      ("automake" ,automake)))
   (inputs
    `(("hwloc" ,hwloc "lib")
      ("gnuplot" ,gnuplot)
      ("mpi" ,nmad)))
   (synopsis "dummy")
   (description "Dummy")
   (license license:lgpl2.0)))

(define-public mpibenchmark
  mpibenchmark-2020-11-26)
