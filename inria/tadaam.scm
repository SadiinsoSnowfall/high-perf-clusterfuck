;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2018 Inria

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
  #:use-module (inria eztrace)
  #:use-module (inria simgrid)
  #:use-module (ice-9 match))

(define %pm2-home-page "http://pm2.gforge.inria.fr/")
(define %pm2-svn "https://scm.gforge.inria.fr/anonscm/svn/pm2/trunk")
(define %padicotm-svn "https://scm.gforge.inria.fr/anonscm/svn/padico/PadicoTM/trunk")
(define %patch-path "inria/patches/autogen_building-tools.patch")

(define %v2019-05-13 "2019-05-13")
(define %v2019-05-13-pm2-revision 28048)
(define %v2019-05-13-padicotm-revision 5248)

(define-public puk-2019-05-13
  (package
   (name "puk")
   (version %v2019-05-13)
   (home-page (string-append %pm2-home-page "/PadicoTM"))
   (source (origin
            (method svn-fetch)
            (uri (svn-reference
                  (url (string-append %padicotm-svn "/PadicoTM/Puk"))
                  (revision %v2019-05-13-padicotm-revision)))
            (sha256
             (base32 "1zy8wgwns1i0hazc87mfwizzyaq4c0zyhd92amaw20wflfnchwb3"))
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
  puk-2019-05-13)

(define-public pioman-2019-05-13
  (package
   (name "pioman")
   (version %v2019-05-13)
   (home-page (string-append %pm2-home-page "/pioman"))
   (source (origin
            (method svn-fetch)
            (uri (svn-reference
                  (url (string-append %pm2-svn "/pioman"))
                  (revision %v2019-05-13-pm2-revision)))
            (sha256
             (base32 "1b8c1ys3vg49pnzcvjzxpygw242dn1z4xprjg971181343bbgc20"))
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
  pioman-2019-05-13)

(define-public padicotm-2019-05-13
  (package
   (name "padicotm")
   (version %v2019-05-13)
   (home-page (string-append %pm2-home-page "/PadicoTM"))
   (source (origin
            (method svn-fetch)
            (uri (svn-reference
                  (url (string-append %padicotm-svn "/PadicoTM/PadicoTM"))
                  (revision %v2019-05-13-padicotm-revision)))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32 "1ry745qc1acr7kwri7yzhyf8pg7lp4hg2mxakqqyx4wdpkpixyr9"))
            (patches (search-patches %patch-path))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
                          "--disable-debug"
                          "--with-pioman"
                          "--without-pukabi")
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
      ("automake" ,automake)
      ("hwloc" ,hwloc "lib")
      ("rdma-core" ,rdma-core)
      ("psm" ,psm)
      ("psm2" ,psm2)))
   (propagated-inputs
    `(("puk" ,puk)
      ("pioman" ,pioman)))
   (synopsis "dummy")
   (description "Dummy")
   (license license:lgpl2.0)))

(define-public padicotm
  padicotm-2019-05-13)

(define-public padicotm-mini-2019-05-13
  (package
   (inherit padicotm)
   (name "padicotm-mini")
   (arguments
    (substitute-keyword-arguments (package-arguments padicotm)
      ((#:configure-flags flags '())
       `(cons "--without-pioman" (delete "--with-pioman" ,flags)))))
   (propagated-inputs
    `(("puk" ,puk)))))

(define-public padicotm-mini
  padicotm-mini-2019-05-13)

(define-public nmad-2019-05-13
  (package
   (name "nmad")
   (version %v2019-05-13)
   (home-page (string-append %pm2-home-page "/NewMadeleine"))
   (source (origin
            (method svn-fetch)
            (uri (svn-reference
                  (url (string-append %pm2-svn "/nmad"))
                  (revision %v2019-05-13-pm2-revision)))
            (sha256
             (base32 "1i4cxan90rg0ipy7c5w3f93k6gpgx38x75b3gfy9wprjf6x0ybwm"))
            (patches (search-patches %patch-path))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
                          "--disable-debug"
                          "--with-pioman"
                          "--without-pukabi"
                          "--enable-mpi"
                          "--disable-sampling")
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
  nmad-2019-05-13)

(define-public nmad-mini-2019-05-13
  (package
   (inherit nmad)
   (name "nmad-mini")
   (arguments
    (substitute-keyword-arguments (package-arguments nmad)
      ((#:configure-flags flags '())
       `(cons "--without-pioman" (delete "--with-pioman" ,flags)))))
   (propagated-inputs
    `(("padicotm" ,padicotm-mini)
      ,@(delete `("padicotm" ,padicotm) (package-inputs nmad))))))

(define-public nmad-mini
  nmad-mini-2019-05-13)
